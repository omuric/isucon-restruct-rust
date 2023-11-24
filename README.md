# isucon-restruct-rust

Decompose rust code into appropriate granularity.
This is a tool designed for use with ISUCON.

## Usage

Make changes directly to the target repository.
Please make it recoverable via git, etc. before executing.

```bash
cargo run -- --path ../isucon13-practice
cd ../isucon13-practice
cargo fix --allow-dirty && cargo fmt
```