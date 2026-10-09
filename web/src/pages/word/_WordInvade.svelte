<script lang="ts">
  import type { Heteronym } from "common";
  export let heteronyms: Heteronym[];
  import Pronunciation from "$src/components/Pronunciation.svelte";
  import Property from "$src/components/Property.svelte";
</script>

{#each heteronyms as het}
  <h1>{het.title}</h1>
  <Pronunciation>{het.props.bopomofo}</Pronunciation>
  <Property key="分類" value={het.props.category}></Property>
  <Property key="粗俗" value={het.props.explicit}></Property>
  {#if het.props.description}
    <p class="def">{het.props.description}</p>
  {/if}
  {#if het.props.notice}
    <div class="text-sm text-secondary-900 dark:text-secondary-600">
      🛈 {het.props.notice}
    </div>
  {/if}
  {#if het.props.examples}
    <h2 class="mb-2">用法與例句</h2>
    <div class="divide-y">
      {#each het.props.examples as example}
        <div>
          <h3>◉ {example.words.join("、")}</h3>
          <Property key="台灣華語" value={example.correct} html={true}
          ></Property>
          <Property key="中國華語" value={example.incorrect} html={true}
          ></Property>
        </div>
      {/each}
    </div>
  {/if}
{/each}
