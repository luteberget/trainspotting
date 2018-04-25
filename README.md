### Railway performance analysis tools

Check out the [documentation with examples](https://luteberget.github.io/rollingdocs).

This repository contains the following programs:

 * **docs** - HTML documentation and examples with interactive visualizations (compile with [mdbook](https://github.com/rust-lang-nursery/mdBook))
 * **rolling** - a simple train simulator written in Rust
 * **railperfcheck** - performance specification checker for railways written in Haskell (based on the rolling simulator, [minisat](http://minisat.se/), and [satplus](https://github.com/koengit/satplus))
 * **gridvis** - graph layout solver in railway style written in Haskell for the infrastructure format in rolling (based on [minisat](http://minisat.se/) and [satplus](https://github.com/koengit/satplus))
 * **rollingrailml** - convert railML 2.x files into rolling infrastructure model. Can also derive routes from infrastructure. (Rust)

### Build instructions

1. Install Rust tools version >= 1.24
2. Install `mdbook` (`cargo install mdbook`)
3. Install Haskell compiler and package tool
4. Install Haskell packages `cabal install megaparsec` and `cabal install cmdargs`
5. Put [satplus](https://github.com/koengit/satplus.git) in subfolder of `railperfcheck` and `gridvis` directories
6. Run `make`.
7. Open `docs/book/index.html` in web browser

