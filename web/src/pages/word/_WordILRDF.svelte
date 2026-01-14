<script lang="ts">
  import type { Heteronym } from "common";
  export let lang: string;
  export let heteronyms: Heteronym[] = [];
  function processDef(str: string, title: string) {
    let buf = "";
    buf += "<ol>";
    let items = str.replace(/\.\.\./g, "…").split(/\d\.[ \n]/g);
    buf += items
      .map((x) => x.replace(/([.。?？])/g, "$1<br>"))
      .map((x) => (x.length > 0 ? `<li>${x}</li>` : ""))
      .join("")
      .replace(/;/g, "；")
      .replace(/没/g, "沒") // typo (IMO)
      .replace(/a href="(\/word\/.*?)"/g, `a href="$1?lang=${lang}"`)
      .replace(RegExp(`([ .])(${title})([ .])`, "ig"), `$1<b>$2</b>$3`);
    buf += "</ol>";
    return buf;
  }
  function processNote(str: string) {
    return str.replace(/(\d)\. /g, "<br>$1. ");
  }
</script>

{#each heteronyms as het}
  <div class="flex w-full items-center">
    <h1>{het.title}</h1>
    {#if typeof het.props.freq === "number"}
      <span class="text-xs text-slate-500">{"★".repeat(het.props.freq)}</span>
    {/if}
  </div>
  {#if het.props.def}
    <!-- JSON: {JSON.stringify(het.props.def)} -->
    {#if het.props.ref}
      <p>參照 {@html het.props.ref}</p>
    {/if}
    <p>
      {@html processDef(het.props.def, het.title)}
    </p>
  {/if}
  {#if het.props.note}
    <blockquote class="mt-2">
      <h3>備註</h3>
      <p>{@html processNote(het.props.note)}</p>
    </blockquote>
  {/if}
{/each}
