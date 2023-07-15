<script>
  export let heteronyms;
  import { spc, newline_string_to_ol } from "$lib/processing";
  import Pronunciation from "$src/components/Pronunciation.svelte";

  function markers(str) {
    if (typeof str !== "string") return;
    const alist = [
      [/([^>])?(※)/g, "所取典源尚有疑慮"],
      [/([^>])?(＃)/g, "所取典源與既有成語辭書有所參差"],
      [/([^>])?(◎)/g, "除主要典源外另收又見資料"],
      [/([^>])?(△)/g, "另有可互見參酌之其他成語"],
      [/([^>])?(■)/g, "有至少一筆與主要典源內容不同的參考資料"],
    ];
    for (const [k, v] of alist) {
      str = str.replace(
        k,
        `$1<a
title="${v}"
href="https://dict.idioms.moe.edu.tw/pageView.jsp?ID=41"
target="_blank"
rel="noreferrer"
>$2</a>`
      );
    }
    return str;
  }

  function nuance(str) {
    /* Sure... */
    return `<table>${str
      .split("\n")
      .map((line) => {
        let s = line.split(",");
        if (s.length == 2) {
          return `<tr>${s
            .map((x) => `<th>${x.replace("ㄨ", "☓")}</th>`)
            .join("")}<th>例句</th></tr>`;
        } else {
          return `<tr>${s
            .map((x) => `<td>${x.replace("ㄨ", "☓")}</td>`)
            .join("")}</tr>`;
        }
      })
      .join("")}</table>`;
  }

  function source(str) {
    return markers(str)
      .split("\n")
      .map((x) =>
        x.replace(/^\*(\d)\*(.*)/, `$2<sup><a href="#sc$1">$1</a></sup>`)
      )
      .join("");
  }

  function source_comment(str) {
    if (typeof str !== "string") return;
    return `<ol>
    ${markers(str)
      .split("\n")
      .map(
        (d, i) =>
          `<li id="sc${i + 1}">${d.replace(
            /^\d+\./,
            ""
          )}<a href="#isc">↩</a></li>`
      )
      .join("")}
</ol>`;
  }
</script>

{#each heteronyms as het}
  <h1>{het.title}</h1>
  {#if het.props.bopomofo}
    <Pronunciation>{spc(het.props.bopomofo)}</Pronunciation>
  {/if}
  <p class="def">{@html markers(het.props.def)}</p>
  {#if het.props.用法語意說明 || het.props.用法使用類別 || het.props.用法例句}
    <h2>用法</h2>
    <p>{het.props.用法語意說明}</p>
    <p>{het.props.用法使用類別}</p>
    {#if het.props.用法例句}
      <h3>例句</h3>
      <p>{@html newline_string_to_ol(het.props.用法例句)}</p>
    {/if}
  {/if}
  <h2>辨識</h2>
  {#if het.props.近義同}
    <p>近義：{@html het.props.近義同}</p>
  {/if}
  {#if het.props.近義反}
    <p>反義：{@html het.props.近義反}</p>
  {/if}
  {#if het.props.word_ref}
    <p>參考詞語：「{@html het.props.word_ref}」</p>
  {/if}
  <p>
    {#if het.props.辨識同}{het.props.辨識同}{/if}
    {#if het.props.辨識異}{het.props.辨識異}{/if}
  </p>
  {#if het.props.辨識例句}
    {@html nuance(het.props.辨識例句)}
  {/if}
  {#if het.props.形音辨誤}
    <p>{het.props.形音辨誤}</p>
  {/if}
  {#if het.props.source_name}
    <h2 id="isc">典源</h2>
    <p>{@html markers(het.props.source_name)}</p>
    <p>{@html source(het.props.source_content)}</p>
    <h3>注解</h3>
    <p>{@html source_comment(het.props.source_comment)}</p>
    {#if het.props.source_reference}
      <p>{@html markers(het.props.source_reference)}</p>
    {/if}
  {/if}
  <h2>典故說明</h2>
  <p>{@html het.props.典故說明}</p>
  {#if het.props.書證}
    <h2>書證</h2>
    {@html newline_string_to_ol(het.props.書證)}
  {/if}
{/each}
