use clap::{Parser, Subcommand};
use dotenv::dotenv;
use ssh2::Session;
use std::io::Read;
use std::net::TcpStream;
use std::process::exit;

/// Simple program to greet a person
#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    #[clap(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
pub enum Command {
    /// Start rust-gen-cloud on all machines
    Start,
    /// Restart rust-gen-cloud on all machines
    Restart,
    /// Stop rust-gen-cloud on all machines
    Stop,
    /// Status of rust-gen-cloud on all machines
    Status,
}

impl Command {
    pub fn commands_str(&self) -> String {
        self.commands().join(" && ")
    }

    pub fn commands(&self) -> Vec<&'static str> {
        match self {
            Command::Start => {
                vec![
                    "source ~/.profile",
                    "cd ~/rust-gen/rust-gen-cloud",
                    "(nohup cargo run --release &> logs.txt &)",
                ]
            }
            Command::Restart => Command::Stop
                .commands()
                .into_iter()
                .chain(Command::Start.commands().into_iter())
                .collect(),
            Command::Stop => {
                vec!["pkill -x rust-gen-cloud"]
            }
            Command::Status => {
                vec!["pgrep rust-gen-cloud"]
            }
        }
    }
}

fn main() {
    dotenv().ok();
    let args = Args::parse();
    let user = std::env::var("SSH_USERNAME").expect("SSH_USERNAME must be set");
    let pwd = std::env::var("SSH_PASSWORD").expect("SSH_PASSWORD must be set");
    let machines = [
        "cloud-vm-42-163.doc.ic.ac.uk",
        "cloud-vm-41-252.doc.ic.ac.uk",
        "cloud-vm-42-30.doc.ic.ac.uk",
    ];
    let mut success = true;
    for machine in machines {
        let tcp = TcpStream::connect(&format!("{}:22", machine)).unwrap();
        let mut sess = Session::new().unwrap();
        sess.set_tcp_stream(tcp);
        sess.handshake().unwrap();

        sess.userauth_password(&user, &pwd).unwrap();
        assert!(sess.authenticated());
        let mut channel = sess.channel_session().unwrap();

        println!("{}: {}", machine, args.command.commands_str().as_str());
        channel.exec(args.command.commands_str().as_str()).unwrap();
        let mut s = String::new();
        channel.read_to_string(&mut s).unwrap();
        channel.wait_close().unwrap();
        let exit_code = channel.exit_status().unwrap();

        match (&args.command, exit_code) {
            (cmd @ Command::Status, 0 | 1) => {
                println!("Successfully run command {:?} for machine {}: {}", cmd, machine, (exit_code == 0).then(||"active").unwrap_or("inactive"));
            }
            (cmd, 0) => {
                println!("Successfully run command {:?} for machine {}", cmd, machine)
            }
            (cmd, exit_code) => {
                success = false;
                eprintln!(
                    "Error executing {:?} for machine {}: exit code {}",
                    cmd, machine, exit_code
                );
            }
        }
    }

    if success {
        println!("Successfully run command: {:?}", args.command);
    } else {
        println!("Failed to run command: {:?}", args.command);
        exit(1);
    }
}
