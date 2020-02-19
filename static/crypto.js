window.onload = function () {
    scrypt_module_factory(function (scrypt) {
        window.scrypt = scrypt;
    });
}

password_hash = function(pwd, username) {
    return scrypt.to_hex(scrypt.crypto_scrypt(scrypt.encode_utf8(pwd),
                scrypt.encode_utf8(username),
                16384, 8, 1, 64));
}