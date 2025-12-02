import socket

REQUEST_FILE = "requests.txt"
RESPONSE_FILE = "responses.txt"

def main(host="127.0.0.1", port=5000):
    # Đọc lệnh từ file
    with open(REQUEST_FILE, "r", encoding="utf-8") as f:
        commands = [line.strip() for line in f if line.strip()]

    with socket.create_connection((host, port)) as sock, \
         open(RESPONSE_FILE, "w", encoding="utf-8") as out:

        # dùng file-like interface để đọc dòng
        sf = sock.makefile("r", encoding="utf-8")

        for cmd in commands:
            print(f"Sending: {cmd}")
            sock.sendall((cmd + "\n").encode("utf-8"))

            # nhận trả lời
            resp = sf.readline()
            if not resp:
                print("Server closed connection.")
                break

            resp = resp.rstrip("\n")
            print(f"Response: {resp}")
            out.write(f"{cmd} ==> {resp}\n")

            if cmd.upper().startswith("QUIT"):
                break

if __name__ == "__main__":
    main()
