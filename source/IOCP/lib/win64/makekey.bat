@echo 创建证书
%构建根证书%
openssl rand -out ssl.rand 1000

%构建根证书私钥%
openssl genrsa -aes256 -out ssl.ca.key.pem 2048

%生成根证书签发申请%
openssl req -new -key ssl.ca.key.pem -out ssl.ca.csr -subj "/C=CN/ST=BJ/L=BJ/O=lesaas/OU=lesaas/CN=*.lesaas.cn" -config openssl.cnf

%签发根证书自行签发根证书%
@echo 签发X.509格式证书命令
openssl x509 -req -days 10000 -sha1 -extensions v3_ca -signkey ssl.ca.key.pem -in ssl.ca.csr -out ssl.certs.ca.cer

pause
