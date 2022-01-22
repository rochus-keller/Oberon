convert binary files like image.jpg with the following Linux command
to a hex string
hexdump -v -e '16/1 "%02X " "\n"' image.jpg
