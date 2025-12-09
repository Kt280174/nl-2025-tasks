#import <Foundation/Foundation.h>
#import <sys/socket.h>
#import <netinet/in.h>
#import <arpa/inet.h>
#import <unistd.h>
#import <pthread.h>

// =========================
// SERVER THREAD
// =========================
void* runServer(void* arg) {

    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

    int server_fd = socket(AF_INET, SOCK_STREAM, 0);

    struct sockaddr_in address;
    address.sin_family = AF_INET;
    address.sin_addr.s_addr = INADDR_ANY;
    address.sin_port = htons(5000);

    bind(server_fd, (struct sockaddr*)&address, sizeof(address));
    listen(server_fd, 1);

    NSLog(@"[SERVER] Listening on port 5000...");

    socklen_t addrlen = sizeof(address);
    int client_fd = accept(server_fd, (struct sockaddr*)&address, &addrlen);
    NSLog(@"[SERVER] Client connected.");

    char buffer[4096] = {0};
    ssize_t r = read(client_fd, buffer, sizeof(buffer));  // FIX unused warning
    (void)r; // explicitly ignore

    NSString *jsonStr = [NSString stringWithUTF8String:buffer];
    NSData *jsonData = [jsonStr dataUsingEncoding:NSUTF8StringEncoding];

    NSError *jsonErr = nil;
    NSDictionary *dict =
        [NSJSONSerialization JSONObjectWithData:jsonData
                                        options:0
                                          error:&jsonErr];

    if (jsonErr != nil) {
        NSLog(@"[SERVER] JSON Parse Error: %@", jsonErr);
        close(client_fd);
        return NULL;
    }

    NSString *op = [dict objectForKey:@"operation"];
    NSArray *numbers = [dict objectForKey:@"numbers"];

    double result = 0;

    if ([op isEqualToString:@"sum"]) {
        for (NSNumber *n in numbers)
            result += [n doubleValue];
    }
    else if ([op isEqualToString:@"avg"]) {
        for (NSNumber *n in numbers)
            result += [n doubleValue];
        result /= [numbers count];
    }
    else if ([op isEqualToString:@"max"]) {
        for (NSNumber *n in numbers) {
            double v = [n doubleValue];
            if (v > result) result = v;
        }
    }

    NSDictionary *resp =
        [NSDictionary dictionaryWithObjectsAndKeys:
            [NSNumber numberWithDouble:result], @"result",
        nil];

    NSError *errWriteJSON = nil;
    NSData *respData =
        [NSJSONSerialization dataWithJSONObject:resp
                                        options:0
                                          error:&errWriteJSON];

    ssize_t w = write(client_fd, [respData bytes], [respData length]); // safe
    (void)w;

    NSLog(@"[SERVER] Sent JSON: %@", resp);

    close(client_fd);
    [pool drain];
    return NULL;
}

// =========================
// MAIN PROGRAM (CLIENT)
// =========================
int main() {

    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

    // Start server
    pthread_t tid;
    pthread_create(&tid, NULL, runServer, NULL);

    sleep(1); // let server boot

    // ------------------- CLIENT LOAD FILE -------------------
    NSString *path = @"/uploads/request.json";
    NSData *fileData = [NSData dataWithContentsOfFile:path];

    if (!fileData) {
        NSLog(@"[CLIENT] Cannot read request.json");
        return 1;
    }

    NSError *jsonErr = nil;
    NSDictionary *reqObj =
        [NSJSONSerialization JSONObjectWithData:fileData
                                        options:0
                                          error:&jsonErr];

    if (jsonErr != nil) {
        NSLog(@"[CLIENT] JSON file parse error: %@", jsonErr);
        return 1;
    }

    NSLog(@"[CLIENT] Loaded JSON from file: %@", reqObj);

    NSError *errReq = nil;
    NSData *reqData =
        [NSJSONSerialization dataWithJSONObject:reqObj
                                        options:0
                                          error:&errReq];

    // ------------------- CLIENT SOCKET -------------------
    int sock = socket(AF_INET, SOCK_STREAM, 0);

    struct sockaddr_in serv_addr;
    serv_addr.sin_family = AF_INET;
    serv_addr.sin_port = htons(5000);
    inet_pton(AF_INET, "127.0.0.1", &serv_addr.sin_addr);

    connect(sock, (struct sockaddr*)&serv_addr, sizeof(serv_addr));

    ssize_t w = write(sock, [reqData bytes], [reqData length]);
    (void)w;

    char buffer[4096] = {0};
    ssize_t r = read(sock, buffer, sizeof(buffer));
    (void)r;

    NSString *respStr =
        [NSString stringWithUTF8String:buffer];

    NSData *respData = [respStr dataUsingEncoding:NSUTF8StringEncoding];

    NSError *errResp = nil;
    NSDictionary *respDict =
        [NSJSONSerialization JSONObjectWithData:respData
                                        options:0
                                          error:&errResp];

    NSLog(@"[CLIENT] Received: %@", respDict);

    close(sock);
    [pool drain];
    return 0;
}
