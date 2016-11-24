DIOCP 修改版（YangYxd)

Demo 源码需要重新设置下搜索路径，指向 Source\IOCP 文件夹即可。 
 
【感谢】
 感谢DIOCP原作者天地弦，以及DIOCP官方群、QDAC官方群所有参与本项目测试、修改的朋友。
 
【注意事项】
 您在使用中发现任何BUG、更改意见等请及时联系我们，不胜感激。
 使用本源码时您需要自行承担一切后果，作者和项目组不为此承担任何法律责任。
 您可以任意复制、更新本源码，但请注明来源，或在您的软件特定位置标注说明。
 更新内容请查看 source 目录下的 ReadMe.txt
 
【OpenSSL】

 如果需要使用HTTPS服务，请将 lib 目录中的 lib.zip 复制到项目根目录中解压。在 lib.zip 中保存了使用 OpenSSL 的dll文件。
 内部的 MakeCA.bat 可以用来生成证书。
 
【联系我们】
  yangyxd QQ: 2514718952 
  天地弦QQ： 185511468  (DIOCP原作者)
  DIOCP 官方群: 320641073
  QDAC 官方群: 250530692
 
【网站】
  yangyxd blog: http://www.cnblogs.com/yangyxd
  DIOCP 官方网站: http://www.diocp.org
  QDAC 官方网站: http://www.qdac.cc
 
【捐助】
  感谢大家的使用和测试，如果你想要捐助我，可以联系QQ 2514718952。
 
【与原版的区别】
1. 简化使用接口。使用时只需要引用 iocp.pas 单元即可。
2. 去除重复代码。
3. 使用了一些自己编写的新类替换掉原来的类似模块，如 TIocpStream, TIocpHttpServer 等
4. 用法更简单了。 

