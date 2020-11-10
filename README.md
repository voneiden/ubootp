# µbootp

µbootp is based on an original idea that, sort of reinvented the wheel. The goal here
is to be able to get a dynamic IP for a IoT device without needing to depend on bloated
DHCP implementations. 1 kilobyte of FLASH? No problem.

[Bootstrap Protocol](https://en.wikipedia.org/wiki/Bootstrap_Protocol) from 1985 implemented
a fairly lightweight protocol for assigning IP addresses for MAC addresses over UDP. 

µbootp is as light as it can get. It uses UDP with a predetermined unicast IP address and port to send a packet that consists of

```
Client: [Q] + [{MAC ADDRESS}] (1+6 = 7 bytes)
Server: [A] + [{MAC ADDRESS}] [{IPv4 ADDRESS}] [{IPv4 NETMASK}] [{IPv4 GATEWAY}] (1+6+4+4+4 = 19 bytes)
```

The MAC->IPv4 mappings are defined in a TOML file.


Pull requests are not accepted as this is a personal project. Feel free to steal the idea. 
