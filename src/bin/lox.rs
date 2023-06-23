use anyhow::Result;
use clap::Parser;
use std::io::BufRead;
use std::io::Write;

#[derive(Parser, Debug)]
struct CliArgs {
    #[arg(short, long)]
    script: Option<String>,
}

fn run_code(_code: String) -> Result<()> {
    unimplemented!()
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
