# EverParse: Veriﬁed Secure Zero-Copy Parsers for Authenticated Message Formats

| Field       | Value |
| ----------- | ----- |
| Authors     | Tahina Ramananandro, Antoine Delignat-Lavaud, Cédric Fournet, Nikhil Swamy, Tej Chajed, Nadim Kobeissi, Jonathan Protzenko |
| Conference  | 28th USENIX Security Symposium |
| Date        | 2019 |
| ISBN        | [978-1-939133-06-9](https://www.usenix.org/conference/usenixsecurity19/presentation/delignat-lavaud) |
| PDF Link    | <https://www.usenix.org/system/files/sec19-ramananandro_0.pdf> |

## Abstract

We present EverParse, a framework for generating parsers
and serializers from tag-length-value binary message format
descriptions. The resulting code is verified to be safe (no
overflow, no use after free), correct (parsing is the inverse of
serialization) and non-malleable (each message has a unique
binary representation). These guarantees underpin the security
of cryptographic message authentication, and they enable
testing to focus on interoperability and performance issues.

## Notes

[@brendanzab](https://github.com/brendanzab):

TODO
