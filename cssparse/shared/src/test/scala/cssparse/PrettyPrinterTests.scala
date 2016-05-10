package cssparse

import utest._

object PrettyPrinterTests extends TestSuite {
  val tests = this {
    'basic {

      val output =
        """
          |html {
          |  font-family: sans-serif;
          |  -webkit-text-size-adjust: 100%;
          |  -ms-text-size-adjust: 100%;
          |}
          |
          |body {
          |  margin: 0;
          |}
          |
          |article, aside, details, figcaption, figure, footer, header, hgroup, main, menu, nav, section, summary {
          |  display: block;
          |}
          |
          |audio, canvas, progress, video {
          |  display: inline-block;
          |  vertical-align: baseline;
          |}
          |
          |audio:not([controls]) {
          |  display: none;
          |  height: 0;
          |}
          |
          |[hidden], template {
          |  display: none;
          |}
          |
          |a {
          |  background-color: transparent;
          |}
          |
          |a:active, a:hover {
          |  outline: 0;
          |}
          |
          |abbr[title] {
          |  border-bottom: 1px dotted;
          |}
          |
          |b, strong {
          |  font-weight: bold;
          |}
          |
          |dfn {
          |  font-style: italic;
          |}
          |
          |h1 {
          |  margin: .67em 0;
          |  font-size: 2em;
          |}
          |
          |mark {
          |  color: #000;
          |  background: #ff0;
          |}
          |
          |small {
          |  font-size: 80%;
          |}
          |
          |input[type="checkbox"], input[type="radio"] {
          |  -webkit-box-sizing: border-box;
          |  -moz-box-sizing: border-box;
          |  box-sizing: border-box;
          |  padding: 0;
          |}
          |
          |input[type="number"]::-webkit-inner-spin-button, input[type="number"]::-webkit-outer-spin-button {
          |  height: auto;
          |}
          |
          |input[type="search"] {
          |  -webkit-box-sizing: content-box;
          |  -moz-box-sizing: content-box;
          |  box-sizing: content-box;
          |  -webkit-appearance: textfield;
          |}
          |
          |input[type="search"]::-webkit-search-cancel-button, input[type="search"]::-webkit-search-decoration {
          |  -webkit-appearance: none;
          |}
          |
          |
          |@font-face  {
          |  font-family: 'Glyphicons Halflings';
          |  src: url('../fonts/glyphicons-halflings-regular.eot');
          |  src: url('../fonts/glyphicons-halflings-regular.eot?#iefix') format('embedded-opentype'), url('../fonts/glyphicons-halflings-regular.woff2') format('woff2'), url('../fonts/glyphicons-halflings-regular.woff') format('woff'), url('../fonts/glyphicons-halflings-regular.ttf') format('truetype'), url('../fonts/glyphicons-halflings-regular.svg#glyphicons_halflingsregular') format('svg');
          |}
          |
          |h4 small, .h4 small, h5 small, .h5 small, h6 small, .h6 small, h4 .small, .h4 .small, h5 .small, .h5 .small, h6 .small, .h6 .small {
          |  font-size: 75%;
          |}
          |
          |
          |@media (min-width:768px) {
          |  .lead {
          |    font-size: 21px;
          |  }
          |}
          |
          |
          |@media screen and (max-width:767px) {
          |  .table-responsive {
          |    width: 100%;
          |    margin-bottom: 15px;
          |    overflow-y: hidden;
          |    -ms-overflow-style: -ms-autohiding-scrollbar;
          |    border: 1px solid #ddd;
          |  }
          |
          |  .table-responsive > .table {
          |    margin-bottom: 0;
          |  }
          |
          |  .table-responsive > .table > thead > tr > th, .table-responsive > .table > tbody > tr > th, .table-responsive > .table > tfoot > tr > th, .table-responsive > .table > thead > tr > td, .table-responsive > .table > tbody > tr > td, .table-responsive > .table > tfoot > tr > td {
          |    white-space: nowrap;
          |  }
          |
          |  .table-responsive > .table-bordered {
          |    border: 0;
          |  }
          |
          |  .table-responsive > .table-bordered > thead > tr > th:first-child, .table-responsive > .table-bordered > tbody > tr > th:first-child, .table-responsive > .table-bordered > tfoot > tr > th:first-child, .table-responsive > .table-bordered > thead > tr > td:first-child, .table-responsive > .table-bordered > tbody > tr > td:first-child, .table-responsive > .table-bordered > tfoot > tr > td:first-child {
          |    border-left: 0;
          |  }
          |
          |  .table-responsive > .table-bordered > thead > tr > th:last-child, .table-responsive > .table-bordered > tbody > tr > th:last-child, .table-responsive > .table-bordered > tfoot > tr > th:last-child, .table-responsive > .table-bordered > thead > tr > td:last-child, .table-responsive > .table-bordered > tbody > tr > td:last-child, .table-responsive > .table-bordered > tfoot > tr > td:last-child {
          |    border-right: 0;
          |  }
          |
          |  .table-responsive > .table-bordered > tbody > tr:last-child > th, .table-responsive > .table-bordered > tfoot > tr:last-child > th, .table-responsive > .table-bordered > tbody > tr:last-child > td, .table-responsive > .table-bordered > tfoot > tr:last-child > td {
          |    border-bottom: 0;
          |  }
          |}
          |
          |
          |@-o-keyframes progress-bar-stripes {
          |  from {
          |    background-position: 40px 0;
          |  }
          |
          |  to {
          |    background-position: 0 0;
          |  }
          |}
        """.stripMargin

      def compareWithOutput(input: String): Unit =
        assert(PrettyPrinter.printRuleList(CssRulesParser.ruleList.parse(input).get.value).trim == output.trim)

      'test1 {
        val input =
          """
            |
            |html {
            |  font-family: sans-serif;
            |  -webkit-text-size-adjust: 100%;
            |      -ms-text-size-adjust: 100%;
            |}
            |body {
            |  margin: 0;
            |}
            |article,
            |aside,
            |details,
            |figcaption,
            |figure,
            |footer,
            |header,
            |hgroup,
            |main,
            |menu,
            |nav,
            |section,
            |summary {
            |  display: block;
            |}
            |audio,
            |canvas,
            |progress,
            |video {
            |  display: inline-block;
            |  vertical-align: baseline;
            |}
            |audio:not([controls]) {
            |  display: none;
            |  height: 0;
            |}
            |[hidden],
            |template {
            |  display: none;
            |}
            |a {
            |  background-color: transparent;
            |}
            |a:active,
            |a:hover {
            |  outline: 0;
            |}
            |abbr[title] {
            |  border-bottom: 1px dotted;
            |}
            |b,
            |strong {
            |  font-weight: bold;
            |}
            |dfn {
            |  font-style: italic;
            |}
            |h1 {
            |  margin: .67em 0;
            |  font-size: 2em;
            |}
            |mark {
            |  color: #000;
            |  background: #ff0;
            |}
            |small {
            |  font-size: 80%;
            |}
            |input[type="checkbox"],
            |input[type="radio"] {
            |  -webkit-box-sizing: border-box;
            |     -moz-box-sizing: border-box;
            |          box-sizing: border-box;
            |  padding: 0;
            |}
            |input[type="number"]::-webkit-inner-spin-button,
            |input[type="number"]::-webkit-outer-spin-button {
            |  height: auto;
            |}
            |input[type="search"] {
            |  -webkit-box-sizing: content-box;
            |     -moz-box-sizing: content-box;
            |          box-sizing: content-box;
            |  -webkit-appearance: textfield;
            |}
            |input[type="search"]::-webkit-search-cancel-button,
            |input[type="search"]::-webkit-search-decoration {
            |  -webkit-appearance: none;
            |}
            |@font-face {
            |  font-family: 'Glyphicons Halflings';
            |
            |  src: url('../fonts/glyphicons-halflings-regular.eot');
            |  src: url('../fonts/glyphicons-halflings-regular.eot?#iefix') format('embedded-opentype'), url('../fonts/glyphicons-halflings-regular.woff2') format('woff2'), url('../fonts/glyphicons-halflings-regular.woff') format('woff'), url('../fonts/glyphicons-halflings-regular.ttf') format('truetype'), url('../fonts/glyphicons-halflings-regular.svg#glyphicons_halflingsregular') format('svg');
            |}
            |
            |h4 small,
            |.h4 small,
            |h5 small,
            |.h5 small,
            |h6 small,
            |.h6 small,
            |h4 .small,
            |.h4 .small,
            |h5 .small,
            |.h5 .small,
            |h6 .small,
            |.h6 .small {
            |  font-size: 75%;
            |}
            |@media (min-width: 768px) {
            |  .lead {
            |    font-size: 21px;
            |  }
            |}
            |@media screen and (max-width: 767px) {
            |  .table-responsive {
            |    width: 100%;
            |    margin-bottom: 15px;
            |    overflow-y: hidden;
            |    -ms-overflow-style: -ms-autohiding-scrollbar;
            |    border: 1px solid #ddd;
            |  }
            |  .table-responsive > .table {
            |    margin-bottom: 0;
            |  }
            |  .table-responsive > .table > thead > tr > th,
            |  .table-responsive > .table > tbody > tr > th,
            |  .table-responsive > .table > tfoot > tr > th,
            |  .table-responsive > .table > thead > tr > td,
            |  .table-responsive > .table > tbody > tr > td,
            |  .table-responsive > .table > tfoot > tr > td {
            |    white-space: nowrap;
            |  }
            |  .table-responsive > .table-bordered {
            |    border: 0;
            |  }
            |  .table-responsive > .table-bordered > thead > tr > th:first-child,
            |  .table-responsive > .table-bordered > tbody > tr > th:first-child,
            |  .table-responsive > .table-bordered > tfoot > tr > th:first-child,
            |  .table-responsive > .table-bordered > thead > tr > td:first-child,
            |  .table-responsive > .table-bordered > tbody > tr > td:first-child,
            |  .table-responsive > .table-bordered > tfoot > tr > td:first-child {
            |    border-left: 0;
            |  }
            |  .table-responsive > .table-bordered > thead > tr > th:last-child,
            |  .table-responsive > .table-bordered > tbody > tr > th:last-child,
            |  .table-responsive > .table-bordered > tfoot > tr > th:last-child,
            |  .table-responsive > .table-bordered > thead > tr > td:last-child,
            |  .table-responsive > .table-bordered > tbody > tr > td:last-child,
            |  .table-responsive > .table-bordered > tfoot > tr > td:last-child {
            |    border-right: 0;
            |  }
            |  .table-responsive > .table-bordered > tbody > tr:last-child > th,
            |  .table-responsive > .table-bordered > tfoot > tr:last-child > th,
            |  .table-responsive > .table-bordered > tbody > tr:last-child > td,
            |  .table-responsive > .table-bordered > tfoot > tr:last-child > td {
            |    border-bottom: 0;
            |  }
            |}
            |
            |@-o-keyframes progress-bar-stripes {
            |  from {
            |    background-position: 40px 0;
            |  }
            |  to {
            |    background-position: 0 0;
            |  }
            |}
          """.stripMargin

        compareWithOutput(input)
      }

      'test2 {
        val input = """html{font-family:sans-serif;-webkit-text-size-adjust:100%;-ms-text-size-adjust:100%;}body{margin:0;}article,aside,details,figcaption,figure,footer,header,hgroup,main,menu,nav,section,summary{display:block;}audio,canvas,progress,video{display:inline-block;vertical-align:baseline;}audio:not([controls]){display:none;height:0;}[hidden],template{display:none;}a{background-color:transparent;}a:active,a:hover{outline:0;}abbr[title]{border-bottom:1px dotted;}b,strong{font-weight:bold;}dfn{font-style:italic;}h1{margin:.67em 0;font-size:2em;}mark{color:#000;background:#ff0;}small{font-size:80%;}input[type="checkbox"],input[type="radio"]{-webkit-box-sizing:border-box;-moz-box-sizing:border-box;box-sizing:border-box;padding:0;}input[type="number"]::-webkit-inner-spin-button,input[type="number"]::-webkit-outer-spin-button{height:auto;}input[type="search"]{-webkit-box-sizing:content-box;-moz-box-sizing:content-box;box-sizing:content-box;-webkit-appearance:textfield;}input[type="search"]::-webkit-search-cancel-button,input[type="search"]::-webkit-search-decoration{-webkit-appearance:none;}@font-face{font-family:'Glyphicons Halflings';src:url('../fonts/glyphicons-halflings-regular.eot');src:url('../fonts/glyphicons-halflings-regular.eot?#iefix') format('embedded-opentype'), url('../fonts/glyphicons-halflings-regular.woff2') format('woff2'), url('../fonts/glyphicons-halflings-regular.woff') format('woff'), url('../fonts/glyphicons-halflings-regular.ttf') format('truetype'), url('../fonts/glyphicons-halflings-regular.svg#glyphicons_halflingsregular') format('svg');}h4 small,.h4 small,h5 small,.h5 small,h6 small,.h6 small,h4 .small,.h4 .small,h5 .small,.h5 .small,h6 .small,.h6 .small{font-size:75%;}@media (min-width:768px){.lead{font-size:21px;}}@media screen and (max-width:767px){.table-responsive{width:100%;margin-bottom:15px;overflow-y:hidden;-ms-overflow-style:-ms-autohiding-scrollbar;border:1px solid #ddd;}.table-responsive > .table{margin-bottom:0;}.table-responsive > .table > thead > tr > th,.table-responsive > .table > tbody > tr > th,.table-responsive > .table > tfoot > tr > th,.table-responsive > .table > thead > tr > td,.table-responsive > .table > tbody > tr > td,.table-responsive > .table > tfoot > tr > td{white-space:nowrap;}.table-responsive > .table-bordered{border:0;}.table-responsive > .table-bordered > thead > tr > th:first-child,.table-responsive > .table-bordered > tbody > tr > th:first-child,.table-responsive > .table-bordered > tfoot > tr > th:first-child,.table-responsive > .table-bordered > thead > tr > td:first-child,.table-responsive > .table-bordered > tbody > tr > td:first-child,.table-responsive > .table-bordered > tfoot > tr > td:first-child{border-left:0;}.table-responsive > .table-bordered > thead > tr > th:last-child,.table-responsive > .table-bordered > tbody > tr > th:last-child,.table-responsive > .table-bordered > tfoot > tr > th:last-child,.table-responsive > .table-bordered > thead > tr > td:last-child,.table-responsive > .table-bordered > tbody > tr > td:last-child,.table-responsive > .table-bordered > tfoot > tr > td:last-child{border-right:0;}.table-responsive > .table-bordered > tbody > tr:last-child > th,.table-responsive > .table-bordered > tfoot > tr:last-child > th,.table-responsive > .table-bordered > tbody > tr:last-child > td,.table-responsive > .table-bordered > tfoot > tr:last-child > td{border-bottom:0;}}@-o-keyframes progress-bar-stripes{from{background-position:40px 0;}to{background-position:0 0;}}"""

        compareWithOutput(input)
      }

      'test3 {
        compareWithOutput(output)
      }
    }
  }
}
