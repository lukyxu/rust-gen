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
    /// Index of machine, [default: all machines].
    #[clap(short = 'i', long)]
    machine_index: Option<usize>,
}

#[derive(Debug, Subcommand)]
pub enum Command {
    /// Start rust-gen-cloud
    Start,
    /// Restart rust-gen-cloud
    Restart,
    /// Stop rust-gen-cloud
    Stop,
    /// Status of rust-gen-cloud
    Status,
    /// Updates and restarts rust-gen-cloud
    Update { branch: String },
}

impl Command {
    pub fn commands_str(&self) -> String {
        self.commands().join(" && ")
    }

    pub fn commands(&self) -> Vec<String> {
        match self {
            Command::Start => {
                vec![
                    "source ~/.profile".to_owned(),
                    "cd ~/rust-gen/rust-gen-cloud".to_owned(),
                    "(nohup cargo run --release &> logs.txt &)".to_owned(),
                ]
            }
            Command::Restart => Command::Stop
                .commands()
                .into_iter()
                .chain(Command::Start.commands().into_iter())
                .collect(),
            Command::Stop => {
                vec!["pkill -x rust-gen-cloud".to_owned()]
            }
            Command::Status => {
                vec!["pgrep rust-gen-cloud".to_owned()]
            }
            Command::Update { branch } => vec![
                "cd ~/rust-gen".to_owned(),
                "git fetch --all".to_owned(),
                format!("git checkout {}", branch).to_owned(),
                "git pull".to_owned(),
            ]
            .into_iter()
            .chain(Command::Restart.commands().into_iter())
            .collect(),
        }
    }
}

fn main() {
    dotenv().ok();
    let args = Args::parse();
    let user = std::env::var("SSH_USERNAME").expect("SSH_USERNAME must be set");
    let pwd = std::env::var("SSH_PASSWORD").expect("SSH_PASSWORD must be set");
    let mut machines: Vec<&str> = vec![
        "cloud-vm-42-163.doc.ic.ac.uk",
        "cloud-vm-41-252.doc.ic.ac.uk",
        "cloud-vm-42-30.doc.ic.ac.uk",
        "cloud-vm-42-116.doc.ic.ac.uk",
        "cloud-vm-42-125.doc.ic.ac.uk",
        "cloud-vm-42-197.doc.ic.ac.uk",
    ];
    if let Some(index) = args.machine_index {
        if index >= machines.len() {
            println!("Machine index out of index: {:?}", args.machine_index);
            exit(1);
        }
        machines = vec![machines[index]];
    }
    let mut success = true;
    for (i, machine) in machines.into_iter().enumerate() {
        let index = args.machine_index.unwrap_or(i);
        let tcp = TcpStream::connect(&format!("{}:22", machine)).unwrap();
        let mut sess = Session::new().unwrap();
        sess.set_tcp_stream(tcp);
        sess.handshake().unwrap();

        sess.userauth_password(&user, &pwd).unwrap();
        assert!(sess.authenticated());
        let mut channel = sess.channel_session().unwrap();

        println!(
            "[{}] {}: {}",
            index,
            machine,
            args.command.commands_str().as_str()
        );
        channel.exec(args.command.commands_str().as_str()).unwrap();
        let mut s = String::new();
        channel.read_to_string(&mut s).unwrap();
        channel.wait_close().unwrap();
        let exit_code = channel.exit_status().unwrap();

        match (&args.command, exit_code) {
            (cmd @ Command::Status, 0 | 1) => {
                println!(
                    "[{}] Successfully run command {:?} for machine {}: {}",
                    index,
                    cmd,
                    machine,
                    (exit_code == 0).then(|| "active").unwrap_or("inactive")
                );
            }
            (cmd, 0) => {
                println!(
                    "[{}] Successfully run command {:?} for machine {}",
                    index, cmd, machine
                )
            }
            (cmd, exit_code) => {
                success = false;
                eprintln!(
                    "[{}] Error executing {:?} for machine {}: exit code {}",
                    index, cmd, machine, exit_code
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
