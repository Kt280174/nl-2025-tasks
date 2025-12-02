use std::fs;
use std::sync::mpsc;
use std::thread;

fn is_prime(n: i64) -> bool {
    if n < 2 {
        return false;
    }
    if n == 2 || n == 3 {
        return true;
    }
    if n % 2 == 0 {
        return false;
    }
    let mut i = 3;
    while i * i <= n {
        if n % i == 0 {
            return false;
        }
        i += 2;
    }
    true
}

fn main() {
    let filename = "input_rust.txt";

    let num_threads = thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(1);

    // readread file
    let content = fs::read_to_string(filename).unwrap_or_else(|_| {
        eprintln!("Not found input.txt!");
        std::process::exit(1);
    });

    // Parse number 
    let numbers: Vec<i64> = content
        .lines()
        .filter_map(|l| l.trim().parse().ok())
        .collect();

    if numbers.is_empty() {
        println!("File input.txt has no number.");
        return;
    }

    // Channel 
    let (tx, rx) = mpsc::channel();

    let len = numbers.len();
    let t = num_threads.min(len);
    let chunk_size = (len + t - 1) / t;

    for i in 0..t {
        let start = i * chunk_size;
        if start >= len {
            break;
        }
        let end = ((i + 1) * chunk_size).min(len);

        let chunk = numbers[start..end].to_vec();
        let tx_thread = tx.clone();

        thread::spawn(move || {
            for &num in &chunk {
                let result = if is_prime(num) { "prime" } else { "not prime" };
                tx_thread.send((num, result.to_string())).unwrap();
            }
        });
    }

    drop(tx);

    let mut results: Vec<(i64, String)> = rx.iter().collect();

    // Sort
    results.sort_by_key(|(n, _)| {
        numbers.iter().position(|x| x == n).unwrap()
    });

    for (n, status) in results {
        println!("{} -> {}", n, status);
    }
}
