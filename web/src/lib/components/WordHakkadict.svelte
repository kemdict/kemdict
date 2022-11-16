<script>
  export let entry;
  export let title;
  import { spc, linkify_brackets } from "$lib/common";

  const p_names = ["四縣", "海陸", "大埔", "饒平", "詔安", "南四縣"];
  function has_pronunciations(het) {
    // If `het` has a truthy `p_四縣` prop, for example.
    // p stands for pronunciation.
    p_names.some((name) => het[`p_${name}`]);
  }
  /**
   * Collect all pronunciations for hakkadict.
   * @param {object} het
   * @returns {string}
   */
  function het_p(het) {
    let ret = "";
    for (const name of p_names) {
      let value = het[`p_${name}`];
      if (value) {
        ret += `<p>${value}（${name}）</p>`;
      }
    }
    if (ret === "") {
      return false;
    } else {
      return spc(ret);
    }
  }

  function process_def(def) {
    if (def) {
      let x = "";
      let match;
      for (const d of def.split("\n")) {
        if ((match = d.match(/^\[(.*)\]$/))) {
          if (x.length > 0) {
            x += `</ol>`;
          }
          x += `<p class="pos">${match[1]}</p><ol>`;
        } else {
          if (x.length == 0) {
            x += `<ol>`;
          }
          x += `<li><p class="def">${d.replace(/^\d+\./, "")}</p></li>`;
        }
      }
      x += "</ol>";
      return linkify_brackets(x);
    } else {
      return "";
    }
  }
</script>

{#each entry.heteronyms as het}
  <h1>{title}</h1>
  {#if has_pronunciations(het)}
    <div class="flex"><span>讀音：</span><span>{@html het_p(p)}</span></div>
  {/if}
  {@html process_def(het.definition)}
{/each}
