# Multi-configuration parser library

Nereon configuration tools in native Rust. See
[nereon-models](https://github.com/riboseinc/nereon-models),
[nereon-syntax](https://github.com/nereon-syntax) and
[libnereon](https://github.com/rioseinc/libnereon/README.md)
for background information.

There is a NOC playground application bundled in `noc`. It generates
a simple 2-pane web page with an editable NOC on the left and parsed
results on the right. To get this running use something along the
lines of:

```
git clone git@github.com:riboseinc/rust-nereon.git
cd rust-nereon
cargo build --manifest-path=noc/Cargo.toml
./target/debug/noc -p 8042
```
and point your browser at `http://localhost:8042`

[Documentation](https://docs.rs/nereon)

[Website](https://github.com/riboseinc/nereon)
>>>>>>> Readme
