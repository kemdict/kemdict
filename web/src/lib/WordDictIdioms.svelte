<script>
  export let entry;
  export let title;
  import { spc, newline_string_to_ol } from "$lib/processing";

  function idioms_nuance(str) {
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

  function idioms_source(str) {
    return str
      .split("\n")
      .map((x) => x.replace(/^\*(\d)\*(.*)/, `$2<a href="#sc$1">$1</a>`))
      .join("");
  }

  function idioms_source_comment(str) {
    if (str) {
      return `<ol>
    ${str
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
    } else {
      return str;
    }
  }
</script>

{#each entry.heteronyms as het}
  <h1>{title}</h1>
  {#if het.bopomofo}
    <p>讀音：<span>{spc(het.bopomofo)}</span></p>
  {/if}
  <p class="def">{@html het.definition}</p>
  {#if het.用法語意說明 || het.用法使用類別 || het.用法例句}
    <h2>用法</h2>
    <p>{het.用法語意說明}</p>
    <p>{het.用法使用類別}</p>
    {#if het.用法例句}
      <h3>例句</h3>
      <p>{@html newline_string_to_ol(het.用法例句)}</p>
    {/if}
  {/if}
  <h2>辨識</h2>
  {#if het.近義同}
    <p>近義：{@html het.近義同}</p>
  {/if}
  {#if het.近義反}
    <p>反義：{@html het.近義反}</p>
  {/if}
  {#if het.word_ref}
    <p>參考詞語：「{@html het.word_ref}」</p>
  {/if}
  <p>{het.辨識同}{het.辨識異}</p>
  {#if het.辨識例句}
    {@html idioms_nuance(het.辨識例句)}
  {/if}
  <p>{het.形音辨誤}</p>
  {#if het.source_name}
    <h2 id="isc">典源</h2>
    <p>{het.source_name}</p>
    <p>{@html idioms_source(het.source_content)}</p>
    <h3>注解</h3>
    <p>{@html idioms_source_comment(het.source_comment)}</p>
    <p>{het.source_reference}</p>
  {/if}
  <h2>典故說明</h2>
  <p>{@html het.典故說明}</p>
  <h2>書證</h2>
  {@html newline_string_to_ol(het.書證)}
{/each}
