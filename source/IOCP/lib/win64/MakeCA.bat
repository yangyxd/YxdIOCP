@echo 创建根证书
%构建根证书%
openssl rand -out ca.rand 1000

%构建根证书私钥%
openssl genrsa -aes256 -out ca.key.pem 2048

%生成根证书签发申请%
openssl req -new -key ca.key.pem -config openssl.cfg -out ca.csr -subj "/C=CN/ST=BJ/L=BJ/O=yangyxd/OU=yangyxd/CN=yangyxd"

%签发根证书自行签发根证书%
@echo 签发X.509格式根证书
openssl x509 -req -days 10000 -sha1 -extensions v3_ca -signkey ca.key.pem -in ca.csr -out ca.cer

@echo 构建服务器证书

%构建服务器私钥%
openssl genrsa -aes256 -out ssl.server.pem 2048

%生成服务器证书签发申请%
openssl req -new -key ssl.server.pem -config openssl.cfg -out ssl.server.csr -subj "/C=CN/ST=BJ/L=BJ/O=lesaas/OU=lesaas/CN=yangyxd" 

%签发服务器证书%
openssl x509 -req -days 3650 -sha1 -extensions v3_req -CA ca.cer -CAkey ca.key.pem -CAserial ca.srl -CAcreateserial -in ssl.server.csr -out ssl.server.cer


@echo 构建客户端证书

%构建客户端私钥%
openssl genrsa -aes256 -out ssl.client.pem 2048

%生成客户端证书签发申请%
openssl req -new -key ssl.client.pem -config openssl.cfg -out ssl.client.csr -subj "/C=CN/ST=BJ/L=BJ/O=lesaas/OU=lesaas/CN=yangyxd" 

%签发客户端证书%
openssl ca -days 3650 -in ssl.client.csr -out ssl.client.cer -cert ca.cer -keyfile ca.key.pem

pause
