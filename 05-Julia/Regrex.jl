# Проверка наличия входного файла
input_file = "input.txt"

if !isfile(input_file)
    println("Файл input.txt не найден!")
    exit()
end

# Чтение текста
text = read(input_file, String)

# Регулярные выражения
email_regex = r"[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}"
phone_regex = r"\+?\d[\d\s\-]{7,}\d"

# Поиск совпадений
emails = [m.match for m in eachmatch(email_regex, text)]
phones = [m.match for m in eachmatch(phone_regex, text)]

# Печать результатов
println("Emails found:")
if isempty(emails)
    println("(none)")
else
    for e in emails
        println(e)
    end
end

println("\nPhones found:")
if isempty(phones)
    println("(none)")
else
    for p in phones
        println(p)
    end
end
