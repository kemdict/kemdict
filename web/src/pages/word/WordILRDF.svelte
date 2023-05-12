<script lang="ts">
  import type { Heteronym } from "common";
  export let lang: string;
  export let heteronyms: Heteronym[] = [];
  function processDef(str: string) {
    let x = "";
    x += "<ol>";
    let items = str.split(/\d\.[ \n]/g);
    x += items
      .map((x) => x.replace(/([.。?？])/g, "$1<br>"))
      .map((x) => (x.length > 0 ? `<li>${x}</li>` : ""))
      .join("")
      .replace(/;/g, "；");
    x += "</ol>";
    return x;
  }
  function processNote(str: string) {
    return str.replace(/(\d)\. /g, "<br>$1. ");
  }
</script>

{#each heteronyms as het}
  <h1>{het.title}</h1>
  {#if het.props.def}
    <!-- JSON: {JSON.stringify(het.props.def)} -->
    <p>
      {@html processDef(het.props.def)}
    </p>
  {/if}
  {#if het.props.note}
    <blockquote class="mt-2">
      <h3>備註</h3>
      <p>{@html processNote(het.props.note)}</p>
    </blockquote>
  {/if}
{/each}
