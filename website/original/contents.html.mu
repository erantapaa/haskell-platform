<!DOCTYPE html>
<head>
    {{> header}}
    <title>The Haskell Platform: Contents</title>
</head>
<body>
<div id="header">
<h1 class="title">The Haskell Platform: Contents</h1>
</div>
<h2 id="haskell-batteries-included">Haskell: Batteries Included</h2>
<p><a href="index.html">The Haskell Platform</a> is a comprehensive, robust development environment for programming in <a href="http://haskell.org">Haskell</a>. For new users the platform makes it trivial to get up and running with a full Haskell development environment. For experienced developers, the platform provides a comprehensive, standard base for commercial and open source Haskell development that maximises interoperability and stability of your code.</p>
<p><strong>Download <a href="index.html">The Haskell Platform</a> for your system</strong></p>
<p>To learn more about programming in Haskell:</p>
<ul>
<li>Visit <a href="http://haskell.org">haskell.org</a> - the center of the Haskell community, a comprehensive resource.</li>
<li>Follow <a href="http://learnyouahaskell.com">Learn You a Haskell</a> - an online Haskell tutorial with a sense of humor.</li>
<li>Or jump straight to <a href="http://book.realworldhaskell.org">Real World Haskell</a>, O'Reilly's book on professional Haskell programming.</li>
<li>You can even <a href="http://tryhaskell.org">Try Haskell</a> in your browser.</li>
</ul>
<p>The following components are provided in the latest revision of The Platform (see the <a href="changelog.html">Changelog</a>):</p>
<h2 id="compiler-and-runtime">Compiler and Runtime</h2>
<p><strong><a href="http://haskell.org/ghc">GHC</a></strong></p>
<p>The state-of-the-art optimizing native code compiler for Haskell.</p>
<p><strong><a href="http://www.haskell.org/ghc/docs/latest/html/users_guide/ghci.html">GHCi</a></strong></p>
<p>A bytecode interpreter and interactive REPL environment for Haskell</p>
<p><strong>The GHC runtime</strong></p>
<p>A multicore language runtime (virtual machine), providing fast lightweight threads, parallel sparks and futures, software transactional memory, core affinity control, a parallel garbage collector, and much more.</p>
<h2 id="developer-tools">Developer Tools</h2>
<p>The Platform also comes with the most useful developer tools out of the box, including:</p>
<p><strong><a href="http://haskell.org/cabal/">Cabal</a></strong></p>
<p>Cabal and cabal-install are tools for building and distributing Haskell libraries and programs. With cabal-install you have immediate access to thousands of Haskell libraries and tools on [Hackage] -- you'll be sure to find something interesting.</p>
<p><strong><a href="http://haskell.org/haddock">Haddock</a></strong></p>
<p>Haddock is a high quality documentation tool for Haskell. Comments and types in your code are used to generate indexed and cross-referenced online documentation.</p>
<p><strong><a href="http://www.haskell.org/ghc/docs/latest/html/users_guide/ghci-debugger.html">The GHCi debugger</a></strong></p>
<p>The Platform ships with the GHCi debugger - an interactive, imperative-style debugger for Haskell. Type &quot;:help&quot; in GHCi for more information.</p>
<p><strong>The <a href="http://haskell.org/happy">Happy</a> parser generator</strong></p>
<p>Happy is a yacc-like parser generator for Haskell for constructing efficient parsers.</p>
<p><strong>The <a href="http://haskell.org/alex">Alex</a> lexer generator</strong></p>
<p>Alex is a lex-like lexer generator for Haskell.</p>
<p><strong>The hsc2hs foreign language binding tool</strong></p>
<p>Often you need to call C libraries from Haskell. hsc2hs is a preprocessor for binding Haskell to C that automates much of the work.</p>
<p><strong>The GHC Profiler</strong></p>
<p>The Platform comes with several tools for analyzing your Haskell programs performance and behaviour. Included are time and space profiling tools, and tools for graphically visualizing the memory use and structure of running Haskell programs.</p>
<p><strong>Haskell Code Coverage</strong></p>
<p>The Platform provides HPC - a professional-grade tool generating code coverage information and statistics for Haskell. Code coverage information can tell you how good your test suite is, or what part of your code is executing at any given time.</p>
<h2 id="packages-and-documentation">Packages and Documentation</h2>
<ul>
<li><a href="doc/{{hpVersion}}/start.html">Read the documentation</a></li>
</ul>
{{> footer}}
</body>
</html>
