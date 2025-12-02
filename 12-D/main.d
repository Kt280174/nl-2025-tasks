import std.stdio;
import std.file;
import std.string;

string escapeHtml(string s)
{
    s = s.replace("&", "&amp;");
    s = s.replace("<", "&lt;");
    s = s.replace(">", "&gt;");
    return s;
}

string processLine(string line)
{
    auto trimmed = line.stripRight();

    if (trimmed.length == 0)
        return "";

    if (trimmed.startsWith("# "))
    {
        auto content = escapeHtml(trimmed[2 .. $].strip());
        return "<h1>" ~ content ~ "</h1>";
    }
    if (trimmed.startsWith("## "))
    {
        auto content = escapeHtml(trimmed[3 .. $].strip());
        return "<h2>" ~ content ~ "</h2>";
    }
    if (trimmed.startsWith("### "))
    {
        auto content = escapeHtml(trimmed[4 .. $].strip());
        return "<h3>" ~ content ~ "</h3>";
    }

    auto content = escapeHtml(trimmed);
    return "<p>" ~ content ~ "</p>";
}

void convertMarkdownToHtml(string inputPath, string outputPath)
{
    string md = std.file.readText(inputPath);
    auto lines = md.splitLines();

    File outFile;
    outFile.open(outputPath, "w");

    bool inList = false;

    foreach (line; lines)
    {
        auto trimmed = line.strip();
        auto left = line.stripLeft();

        if (trimmed.length == 0)
        {
            if (inList)
            {
                outFile.writeln("</ul>");
                inList = false;
            }
            continue;
        }

        bool isDash = left.startsWith("- ");
        bool isStar = left.startsWith("* ");

        if (isDash || isStar)
        {
            if (!inList)
            {
                outFile.writeln("<ul>");
                inList = true;
            }

            string itemText = escapeHtml(left[2 .. $].strip());
            outFile.writeln("  <li>", itemText, "</li>");
        }
        else
        {
            if (inList)
            {
                outFile.writeln("</ul>");
                inList = false;
            }

            auto htmlLine = processLine(line);
            if (htmlLine.length > 0)
                outFile.writeln(htmlLine);
        }
    }

    if (inList)
        outFile.writeln("</ul>");

    outFile.close();
}

void main()
{
    // TẠO FILE INPUT BẰNG write() (đảm bảo hoạt động 100%)
    std.file.write("input.txt",
        "# Hello World\n\n" ~
        "This is a test.\n\n" ~
        "## List\n\n" ~
        "- Milk\n" ~
        "- Bread\n" ~
        "- Eggs\n\n" ~
        "Normal text again.\n"
    );

    convertMarkdownToHtml("input.txt", "output.html");

    writeln("Converted input.md -> output.html\n");
    writeln("OUTPUT HTML CONTENT:\n");
    writeln(std.file.readText("output.html"));
}
