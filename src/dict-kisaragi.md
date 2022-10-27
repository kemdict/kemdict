---
layout: md.njk
title: 字典 | Kisaragi's Extras
---

# 字典：Kisaragi's extras

這個「字典」裡包含了我所寫的定義。

以下是裡面的所有詞：

{% for x in kisaragi_dict %}
- [{{ x.title }}]({{ x.title | prepend: "/word/" | url}})
{% endfor %}
