### Railway performance analysis tools

This repository contains the following programs:

 * **rolling** - a simple train simulator written in Rust
 * **railperfcheck** - performance specification checker for railways written in Haskell (based on the rolling simulator, [minisat](http://minisat.se/), and [satplus](https://github.com/koengit/satplus))
 * **gridvis** - graph layout solver in railway style written in Haskell for the infrastructure format in rolling (based on [minisat](http://minisat.se/) and [satplus](https://github.com/koengit/satplus))
 * **docbook** - HTML documentation and examples with interactive visualizations (compile with [mdbook](https://github.com/rust-lang-nursery/mdBook))

### Build instructions

1. Install Rust stable version >= 1.24
2. Run `make` in `rolling` directory
3. Install Haskell compiler and package tool
4. Install Haskell packages `cabal install megaparsec` and `cabal install cmdargs`
5. Put [satplus](https://github.com/koengit/satplus.git) in subfolder of `railperfcheck` and `gridvis` directories
6. Run `make` in `railperfcheck` directory
7. Run `make` in `gridvis` directory
8. Install `mdbook` (`cargo install mdbook`)
9. Run `make` in `docbook` directory
10. Open `docbook/book/index.html` in web browser

