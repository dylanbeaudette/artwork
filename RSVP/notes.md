

## GPG
 * https://gnupg.org/gph/en/manual.html
 * https://gnupg.org/documentation/guides.html
 * https://emailselfdefense.fsf.org/en/
 
 
 
## Command Line Examples

 * https://wiki.openssl.org/index.php/Command_Line_Utilities
 * https://stackoverflow.com/questions/16056135/how-to-use-openssl-to-encrypt-decrypt-files
 * https://superuser.com/questions/1394222/how-can-i-do-asymmetric-encryption-on-the-command-line-without-relying-on-insta


msg='all work and no play makes jack a dull boy.'

Conversion to/from nicely formatted base16:

 * echo $msg | hexdump -v -e '4/1 "%02X " "\n"'
 * echo $msg | hexdump -v -e '4/1 "%02X " "\n"' | xxd -p -r


61 6C 6C 20
77 6F 72 6B
20 61 6E 64
20 6E 6F 20
70 6C 61 79
20 6D 61 6B
65 73 20 6A
61 63 6B 20
61 20 64 75
6C 6C 20 62
6F 79 2E 0A


Better, squish into 2-byte blocks. Must coordinate hexdump formatting with awk 
 * echo $msg | hexdump -v -e '8/1 "%02X " "\n"' | awk -F' ' '{print $1$2, $3$4, $5$6, $7$8}'
 * echo $msg | hexdump -v -e '8/1 "%02X " "\n"' | awk -F' ' '{print $1$2, $3$4, $5$6, $7$8}' | xxd -p -r


616C 6C20 776F 726B
2061 6E64 206E 6F20
706C 6179 206D 616B
6573 206A 6163 6B20
6120 6475 6C6C 2062
6F79 2E0A



 
Encrypt (asdf) and base64 encode the output
 * echo $msg | openssl enc -aes-256-cbc -a -salt -pbkdf2
 
U2FsdGVkX19TfAfagCsFvq8hApWBl//rnEmJMqx9ksp8dyuYmMMTT5+GxKe9hRhl
n3O0jbRKxuqBs/tL5zlfUQ==


Convert from base64 -> Decrypt (asdf)
 * echo "U2FsdGVkX19TfAfagCsFvq8hApWBl//rnEmJMqx9ksp8dyuYmMMTT5+GxKe9hRhl
n3O0jbRKxuqBs/tL5zlfUQ==" | openssl enc -d -aes-256-cbc -pbkdf2 -a

 
Encrypt (asdf) -> base64 -> formatted base16
 * echo $msg | openssl aes-256-cbc -a -salt -pbkdf2 | hexdump -ve '8/1 "%02X " "\n"' | awk -F' ' '{print $1$2, $3$4, $5$6, $7$8}'
 
Must include the trailing newline character
ct='5532 4673 6447 566B
5831 2B36 5277 3446
5851 4C35 7664 7430
7A62 4745 5145 7566
6B58 342F 4B6D 6C37
5368 4F66 752B 5759
5971 2F69 6235 4148
334A 3337 4576 6967
0A35 6471 4A4D 7A77
6473 332F 4B66 3146
5351 6333 7555 673D
3D0A
'

Convert base16 -> base64 -> decrypt (asdf)
 * echo $ct | xxd -p -r | openssl enc -d -aes-256-cbc -pbkdf2 -a
 


Compare with RSVP output

openSSL
-------------------
5532 4673 6447 566B
5831 2B36 5277 3446
5851 4C35 7664 7430
7A62 4745 5145 7566
6B58 342F 4B6D 6C37
5368 4F66 752B 5759
5971 2F69 6235 4148
334A 3337 4576 6967
0A35 6471 4A4D 7A77
6473 332F 4B66 3146
5351 6333 7555 673D
3D0A



RSVP
-------------------
BC85 64BF C934 3FE0
C8B3 CB5D 032E CBCC
0767 F15E 9711 F6F9
7CCE 2458 437B 048E
C01F 8D86 2F43 5AF2
43F3 1562 A9B2 EF13
ECC9 CB5E D39E DC4B
D047 71EF 52BA DFAA



## Arduino
 * https://forum.arduino.cc/t/aes-256-ecb-encryption-decryption-with-arduino-php-openssl/615946/2




## TODO:
 * getting cypher text into a device for decryption




