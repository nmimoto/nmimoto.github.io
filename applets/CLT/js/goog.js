function googleTrack() {
    var goog = "<script async src=\"https://www.googletagmanager.com/gtag/js?id=UA-141388-4\"></script>\n" +
        "    <script>\n" +
        "        window.dataLayer = window.dataLayer || [];\n" +
        "        function gtag(){dataLayer.push(arguments);}\n" +
        "        gtag('js', new Date());\n" +
        "\n" +
        "        gtag('config', 'UA-141388-4');\n" +
        "    </script>";
    document.write(goog);
}