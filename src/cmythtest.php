<html>
<head>
<title>libcmyth php test</title>
<!--
  -- Make sure enable_dl=On and the cmyth PHP library is in extension_dir
  -->
</head>

<body>

<h1>libcmyth php test</h1>

<?php

include("cmyth.php");

$conn = new connection("localhost");
$ver = $conn->protocol_version();

echo "<p>Protocol Version: " . $ver . "</p>/n";

?>

</body>
</html>
