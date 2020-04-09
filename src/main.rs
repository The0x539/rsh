use std::env;

#[derive(Debug, Clone)]
enum Error {
    BackslashAtEnd,
    MultiRedirect,
    Expected(&'static str),
    ParseFail,
}

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone)]
enum Stdin {
    Terminal,
    File(String),
    Pipe,
}

#[derive(Debug, Clone)]
enum Token {
    Argument(String),
    Pipe,
    LeftWaka,
    RightWaka,
    DoubleRightWaka,
    #[allow(dead_code)]
    Substitution(Box<Job>),
    And,
    Or,
    Semicolon,
    Ampersand,
}

#[derive(Debug, Clone)]
enum Stdout {
    Terminal,
    File(String),
    FileAppend(String),
    Pipe(Box<Command>),
}

#[derive(Debug, Clone)]
struct Command {
    argv: Vec<String>,
    stdin: Stdin,
    stdout: Stdout,
}

#[derive(Debug, Clone)]
enum JobCons {
    And(Box<Command>),  // &&
    Or(Box<Command>),   // ||
}

#[derive(Debug, Clone)]
struct Job {
    first: Command,
    rest: Vec<JobCons>,
    background: bool,
}

impl Command {
    pub fn set_stdin(&mut self, stdin: Stdin) -> Result<()> {
        match self.stdin {
            Stdin::Terminal => {
                self.stdin = stdin;
                Ok(())
            },
            _ => Err(Error::MultiRedirect)
        }
    }
    pub fn set_stdout(&mut self, stdout: Stdout) -> Result<()> {
        match self.stdout {
            Stdout::Terminal => {
                self.stdout = stdout;
                Ok(())
            },
            _ => Err(Error::MultiRedirect)
        }
    }
}

fn parse_token(cmdline: &str) -> Result<(Token, &str)> {
    let (token, idx) = None
        .or(take_delimiter(cmdline))
        .or(take_bareword(cmdline)?)
        .ok_or(Error::ParseFail)?;

    Ok((token, cmdline.split_at(idx).1.trim_start()))
}

fn take_delimiter(cmdline: &str) -> Option<(Token, usize)> {
    let symbols = [
        (">>", Token::DoubleRightWaka),
        (">", Token::RightWaka),
        ("<", Token::LeftWaka),
        ("||", Token::Or),
        ("|", Token::Pipe),
        ("&&", Token::And),
        ("&", Token::Ampersand),
        (";", Token::Semicolon),
    ];

    for (sym, tok) in &symbols {
        if cmdline.starts_with(sym) {
            return Some((tok.clone(), sym.len()));
        }
    }

    None
}

fn take_bareword(cmdline: &str) -> Result<Option<(Token, usize)>> {
    let mut idx = 0;
    let mut escaped = false;
    let mut token = String::new();
    for (_, c) in cmdline.char_indices() {
        if escaped {
            idx += c.len_utf8();
            token.push(c);
            escaped = false;
            continue;
        }
        match c {
            ' ' | ';' | '&' | '|' | '<' | '>' => break,
            '\\' => escaped = true,
            _ => token.push(c),
        }
        idx += c.len_utf8();
    }

    if escaped {
        Err(Error::BackslashAtEnd)
    } else if token.is_empty() {
        Ok(None)
    } else {
        Ok(Some((Token::Argument(token), idx)))
    }
}

fn parse_pipeline(mut cmdline: &str, stdin: Stdin) -> Result<(Command, &str)> {
    cmdline = cmdline.trim_start();

    let mut cmd = Command {
        argv: Vec::new(),
        stdin: stdin,
        stdout: Stdout::Terminal,
    };

    while !cmdline.is_empty() {
        let (token, rest) = parse_token(cmdline)?;
        match token {
            Token::Argument(arg) => cmd.argv.push(arg),
            Token::LeftWaka | Token::RightWaka | Token::DoubleRightWaka => {
                let (target, rest2) = parse_token(rest)?;
                match target {
                    Token::Argument(arg) => match token {
                        Token::LeftWaka => cmd.set_stdin(Stdin::File(arg))?,
                        Token::RightWaka => cmd.set_stdout(Stdout::File(arg))?,
                        Token::DoubleRightWaka => cmd.set_stdout(Stdout::FileAppend(arg))?,
                        _ => unreachable!("redirecting stdio using {:?}", token),
                    },
                    _ => return Err(Error::Expected("redirection target")),
                }
                cmdline = rest2;
                continue;
            },
            Token::Pipe => {
                 let (dest, rest2) = parse_pipeline(rest, Stdin::Pipe)?;
                 cmd.set_stdout(Stdout::Pipe(Box::new(dest)))?;
                 cmdline = rest2;
                 break;
            },
            Token::And | Token::Or | Token::Semicolon | Token::Ampersand => break,
            _ => (),
        }
        cmdline = rest;
    }

    Ok((cmd, cmdline))
}

fn parse_job(mut cmdline: &str) -> Result<(Job, &str)> {
    cmdline = cmdline.trim_start();

    let (first_cmd, rest) = parse_pipeline(cmdline, Stdin::Terminal)?;
    cmdline = rest;

    let mut job = Job {
        first: first_cmd,
        rest: Vec::new(),
        background: false,
    };

    while !cmdline.is_empty() {
        let (token, rest) = parse_token(cmdline)?;
        match token {
            Token::And | Token::Or => {
                let (next_cmd, rest2) = parse_pipeline(rest, Stdin::Terminal)?;
                let cons_type = match token {
                    Token::And => JobCons::And,
                    Token::Or => JobCons::Or,
                    _ => unreachable!("chaining commands using {:?}", token),
                };
                job.rest.push(cons_type(Box::new(next_cmd)));
                cmdline = rest2;
            },
            Token::Ampersand | Token::Semicolon => {
                job.background = match token {
                    Token::Ampersand => true,
                    _ => false,
                };
                cmdline = rest;
                break;
            }
            _ => return Err(Error::Expected("pipeline separator or job separator")),
        }
    }

    Ok((job, cmdline))
}

fn parse_cmdline(mut cmdline: &str) -> Result<Vec<Job>> {
    let mut jobs = Vec::new();

    while !cmdline.is_empty() {
        let (job, rest) = parse_job(cmdline)?;
        jobs.push(job);
        cmdline = rest;
    }

    Ok(jobs)
}

fn main() {
    for arg in env::args().skip(1) {
        println!("{:?}", parse_cmdline(&arg));
    }
}
