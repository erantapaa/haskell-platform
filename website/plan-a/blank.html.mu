<!DOCTYPE html> <!-- -*- mode: web-mode; engine: ctemplate -*- -->
<html>
    <head>
        {{> plan-a/head }}
        <link rel="stylesheet" type="text/css" href="stylesheets/contents.css">

        <meta name="description" content="Haskell Platform is a Haskell distribution with batteries included">
        <script src="js/contents.js"></script>
        <title>Haskell Platform - Included Packages</title>
    </head>
    <body class="page-home">
        <div class="wrap">
            <div class="template">
                {{> plan-a/navbar }}

                <div class="header">
                    <div class="container">
                        <div class="row">
                            <div class="span12 col-md-12">
                                <div class="branding">
                                    <span style="background-image: url(img/logo.png)" class="name">Haskell Platform</span>
                                    <span class="summary">
                                        Haskell with batteries included
                                    </span>
                                </div>
                            </div>
                            <div class="span6 col-md-6">
                                <div class="branding">
                                    <span class="tag"></span>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>

                <div class="container">
                    <!-- content goes here -->
                    <h2>Included Packages</h2>
                    ...
                </div>
            </div>
        </div>

        {{> plan-a/footer }}
    </body>
</html>
