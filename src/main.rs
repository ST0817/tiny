use rustyline::{DefaultEditor, Result, error::ReadlineError};

fn main() -> Result<()> {
    let mut editor = DefaultEditor::new()?;
    loop {
        let read = editor.readline(">> ");
        match read {
            Ok(line) => {
                editor.add_history_entry(&line)?;
                println!(" => {}", line);
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
