# EverParse: Veriﬁed Secure Zero-Copy Parsers for Authenticated Message Formats

- **Authors**: Tahina Ramananandro, Antoine Delignat-Lavaud, Cédric Fournet, Nikhil Swamy, Tej Chajed, Nadim Kobeissi, Jonathan Protzenko
- **Conference**: 28th USENIX Security Symposium
- **Date**: 2019
- **ISBN**: [978-1-939133-06-9](https://www.usenix.org/conference/usenixsecurity19/presentation/delignat-lavaud)
- **PDF Link**: <https://www.usenix.org/system/files/sec19-ramananandro_0.pdf>
- **Abstract**:
  > We present EverParse, a framework for generating parsers and serializers
  > from tag-length-value binary message format descriptions. The resulting code
  > is verified to be safe (no overflow, no use after free), correct (parsing is
  > the inverse of serialization) and non-malleable (each message has a unique
  > binary representation). These guarantees underpin the security of
  > cryptographic message authentication, and they enable testing to focus on
  > interoperability and performance issues.
  >
  > EverParse consists of two parts: LowParse, a library of parser combinators
  > and their formal properties written in F*; and QuackyDucky, a compiler from
  > a domain-specific language of RFC message formats down to low-level F* code
  > that calls LowParse. While LowParse is fully verified, we do not formalize
  > the semantics of the input language and keep QuackyDucky outside our trusted
  > computing base. Instead, it also outputs a formal message specification, and
  > F* automatically verifies our implementation against this specification.
  >
  > EverParse yields efficient zero-copy implementations, usable both in F\* and
  > in C. We evaluate it in practice by fully implementing the message formats
  > of the Transport Layer Security standard and its extensions (TLS 1.0--1.3,
  > 293 datatypes) and by integrating them into miTLS, an F* implementation of
  > TLS. We illustrate its generality by implementing the Bitcoin block and
  > transaction formats, and the ASN.1 DER payload of PKCS#1 RSA signatures. We
  > integrate them into C applications and measure their runtime performance,
  > showing significant improvements over prior handwritten libraries.

<!-- markdownlint-disable no-inline-html -->
<details>
<summary>BibTeX</summary>

```bibtex
@inproceedings {236232,
author = {Tahina Ramananandro and Antoine Delignat-Lavaud and Cedric Fournet and
Nikhil Swamy and Tej Chajed and Nadim Kobeissi and Jonathan Protzenko},
title = {{EverParse}: Verified Secure {Zero-Copy} Parsers for Authenticated Message Formats},
booktitle = {28th USENIX Security Symposium (USENIX Security 19)},
year = {2019},
isbn = {978-1-939133-06-9},
address = {Santa Clara, CA},
pages = {1465--1482},
url = {https://www.usenix.org/conference/usenixsecurity19/presentation/delignat-lavaud},
publisher = {USENIX Association},
month = aug
}
```

</details>
<!-- markdownlint-restore -->
