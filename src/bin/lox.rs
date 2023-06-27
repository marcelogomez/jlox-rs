use anyhow::Result;
use clap::Parser;
use std::io::BufRead;
use std::io::Write;

use jlox_rs::lexer::Lexer;

#[derive(Parser, Debug)]
struct CliArgs {
    #[arg(short, long)]
    script: Option<String>,
}

fn run_code(code: String) -> Result<()> {
    let lexer = Lexer::new(&code);

    let (tokens, errors): (Vec<_>, Vec<_>) = lexer.partition(Result::is_ok);

    if !errors.is_empty() {
        eprintln!("{} errors:", errors.len());
        for error in errors {
            eprintln!("{:?}", error);
        }
    } else {
        println!("Success");
        for token in tokens {
            // Unwrap is safe because of the partition above
            println!("{:?}", token.unwrap());
        }
    }

    Ok(())
}

fn run_repl() -> Result<()> {
    print!("> ");
    std::io::stdout().flush()?;

    for line in std::io::stdin().lock().lines() {
        run_code(line?)?;

        print!("> ");
        std::io::stdout().flush()?;
    }

    Ok(())
}

fn run_script(script: String) -> Result<()> {
    run_code(script)
}

fn main() -> Result<()> {
    let args = CliArgs::parse();

    if let Some(script) = args.script {
        run_script(script)?;
    } else {
        run_repl()?;
    }

    Ok(())
}
