mod ast;
mod parse;

use parse::parse;
use rustyline::{DefaultEditor, Result, error::ReadlineError};

fn main() -> Result<()> {
    let mut editor = DefaultEditor::new()?;
    loop {
        let read = editor.readline(">> ");
        match read {
            Ok(line) => {
                editor.add_history_entry(&line)?;
                match parse(&line) {
                    Ok((_, expr)) => println!("{:?}", expr),
                    Err(err) => eprint!("{}", err),
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("Interrupted");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("Quit");
                break;
            }
            Err(err) => {
                eprintln!("Error: {:?}", err);
                break;
            }
        }
    }
    Ok(())
}
