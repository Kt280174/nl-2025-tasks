import std.stdio;
import std.file;
import std.string;
import std.conv : to;

// ===== Node =====
class Node
{
    int value;
    Node left;
    Node right;

    this(int v)
    {
        value = v;
    }
}

// ===== Đọc file và dựng cây =====
Node readTreeFromFile(string filename)
{
    auto f = File(filename, "r");

    Node[int] nodes; 
    int rootValue = -1;

    foreach (line; f.byLine())
    {
        auto s = line.strip();
        if (s.length == 0) continue;

        auto p = s.split();
        int v = p[0].to!int;
        int l = p[1].to!int;
        int r = p[2].to!int;

        if (rootValue == -1)
            rootValue = v;

        if (v !in nodes)
            nodes[v] = new Node(v);

        if (l != -1)
        {
            if (l !in nodes)
                nodes[l] = new Node(l);
            nodes[v].left = nodes[l];
        }

        if (r != -1)
        {
            if (r !in nodes)
                nodes[r] = new Node(r);
            nodes[v].right = nodes[r];
        }
    }

    return nodes[rootValue];
}

// ===== Traversals =====
void preorder(Node n)
{
    if (n is null) return;
    write(n.value, " ");
    preorder(n.left);
    preorder(n.right);
}

void inorder(Node n)
{
    if (n is null) return;
    inorder(n.left);
    write(n.value, " ");
    inorder(n.right);
}

void postorder(Node n)
{
    if (n is null) return;
    postorder(n.left);
    postorder(n.right);
    write(n.value, " ");
}

void levelOrder(Node root)
{
    if (root is null) return;

    Node[] q;
    q ~= root;
    size_t head = 0;

    while (head < q.length)
    {
        auto cur = q[head++];
        write(cur.value, " ");

        if (cur.left !is null)  q ~= cur.left;
        if (cur.right !is null) q ~= cur.right;
    }
}

// ===== ASCII TREE (rotated 90 degrees) =====
void printAscii(Node root, int indent = 0)
{
    if (root is null)
        return;

    // right subtree
    printAscii(root.right, indent + 4);

    // print node
    foreach (_; 0 .. indent)
        write(" ");
    writeln(root.value);

    // left subtree
    printAscii(root.left, indent + 4);
}

// ===== MAIN =====
void main()
{
    Node root = readTreeFromFile("tree.txt");

    writeln("Preorder:");
    preorder(root);
    writeln();

    writeln("Inorder:");
    inorder(root);
    writeln();

    writeln("Postorder:");
    postorder(root);
    writeln();

    writeln("Level-order:");
    levelOrder(root);
    writeln();

    writeln("\nASCII TREE (rotated):");
    printAscii(root);
}
