<h1>Overview</h1>

<p>The financeSMM package uses the <a href="" title="https://github.com/joshuaulrich/quantmod">quantmod</a> package to download data from google and manipulate it. This package provides a number of tools that can be used to analyze the returns of different stocks including:</p>

<ul>
<li>create mean-variance efficient portfolios</li>
<li>create time series of returns</li>
<li>compare the returns of a portfolio with those of an ETF</li>
</ul>

<h1>Installation</h1>

<p><code>
devtools::install_github("SmarshMELLOW/financeSMM")
</code></p>

<p>This package is in the testing phase, so if you encounter a bug, please e-mail me your code and I will work on a patch.</p>

<h1>Usage</h1>

<p>Analyze returns from an artificial portfolio using different weights with an ETF </p>

<pre><code>ITA.desc &lt;- read_ishares( ishares.url, "ITA", save_dir = paste0( proj.dir, "data/") )

ITA.list &lt;- ITA.desc %&gt;%
  filter( asset_class == "Equity" ) %&gt;%  # need to exclude cash!
  mutate( tic.list = paste0(exchange, ":", tic) ) %&gt;%
  select( tic.list ) %&gt;% as.matrix( ) %&gt;% t() %&gt;% as.vector()

ITA.df &lt;- get_close( ITA.list )
</code></pre>

