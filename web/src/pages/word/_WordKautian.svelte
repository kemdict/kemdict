<script lang="ts">
  import type { Heteronym } from "common";
  import type { OutputWord } from "$dicts/ministry-of-education/kautian.ts";
  let { heteronyms }: { heteronyms: Array<Heteronym<OutputWord>> } = $props();
  import Pronunciation from "$src/components/Pronunciation.svelte";
  import Property from "$src/components/Property.svelte";

  let prev單字不成詞 = $state(false);

  /**
   * Join two arrays together then sort them by the `.han` key.
   * DWIM if any of them is undefined.
   */
  function appendAndSort<T extends { han: string }, U extends { han: string }>(
    a: T[] | undefined,
    b: U[] | undefined,
  ): (T | U)[] | undefined {
    if (a === undefined) return b;
    if (b === undefined) return a;
    return [...a, ...b].sort((a, b) => (a.han < b.han ? -1 : 1));
  }
</script>

{#each heteronyms as kautianWord, i}
  {@const thisType = kautianWord.props.type}
  {@const this單字不成詞 = kautianWord.props.type === "單字不成詞者"}

  <div id="kautian-word-{kautianWord.props.id}">
    <!-- 這個是單字不成詞者的話，如果前一個也是那就不要再顯示一次標題 -->
    {#if !this單字不成詞 || !prev單字不成詞}
      <h1>{kautianWord.title}</h1>
    {/if}
    <Pronunciation>{kautianWord.props.tl.main}</Pronunciation>
    <Property key="異用字" value={kautianWord.props.han.alt?.join("、")}
    ></Property>
    {#if kautianWord.props.wwSynonyms}
      <Property key="近義詞">
        {#each kautianWord.props.wwSynonyms as it}
          <a class="block" href="/word/{it.han}#kautian-word-{it.wordId}"
            >{it.han}</a
          >
        {/each}
      </Property>
    {/if}
    {#if kautianWord.props.wwAntonyms}
      <Property key="反義詞">
        {#each kautianWord.props.wwAntonyms as it}
          <a class="block" href="/word/{it.han}#kautian-word-{it.wordId}"
            >{it.han}</a
          >
        {/each}
      </Property>
    {/if}
    <!-- 在有一群單字不成詞之類的詞目時，只在最後一個顯示無義項的說明 -->
    {#if thisType === "單字不成詞者" || thisType === "臺華共同詞" || thisType === "近反義詞不單列詞目者"}
      {@const nextType = heteronyms[i + 1]?.props.type}
      {#if thisType !== nextType}
        {#if thisType === "單字不成詞者"}
          <p class="def">（單字不成詞者 ，無義項）</p>
        {:else if thisType === "臺華共同詞"}
          <p class="def">（臺華共同詞，無義項）</p>
        {:else if thisType === "近反義詞不單列詞目者"}
          <p class="def">（近反義詞不單列詞目）</p>
        {/if}
      {/if}
    {/if}
    {#if (prev單字不成詞 = this單字不成詞)}{/if}

    <ol class="space-y-4">
      {#each kautianWord.props.heteronyms as kautianHet}
        <li id="kautian-het-{kautianHet.id}">
          <p class="def">
            {#if kautianHet.pos}<span class="font-bold"
                >［{kautianHet.pos}］</span
              >
            {/if}{@html kautianHet.def}
          </p>
          {#each kautianHet.examples as example}
            <blockquote>
              <p>{example.han}</p>
              <p>{example.tl}</p>
              <p class="pt-1 opacity-80">({example.zh})</p>
            </blockquote>
          {/each}
          {#if kautianHet.hhSynonyms || kautianHet.hwSynonyms}
            <Property key="近義詞">
              {#each appendAndSort(kautianHet.hhSynonyms, kautianHet.hwSynonyms) as it}
                <a
                  class="block"
                  href={"hetId" in it
                    ? `/word/${it.han}#kautian-het-${it.hetId}`
                    : `/word/${it.han}#kautian-word-${it.wordId}`}>{it.han}</a
                >
              {/each}
            </Property>
          {/if}
          {#if kautianHet.hhAntonyms || kautianHet.hwAntonyms}
            <Property key="反義詞">
              {#each appendAndSort(kautianHet.hhAntonyms, kautianHet.hwAntonyms)?.sort( (a, b) => (a.han < b.han ? -1 : 1), ) as it}
                <a
                  class="block"
                  href={"hetId" in it
                    ? `/word/${it.han}#kautian-het-${it.hetId}`
                    : `/word/${it.han}#kautian-word-${it.wordId}`}>{it.han}</a
                >
              {/each}
            </Property>
          {/if}
        </li>
      {/each}
    </ol>

    {#if kautianWord.props.tl.dialects}
      {@const dialects = kautianWord.props.tl.dialects}
      <Property key="語音差異" class="mt-4">
        <div>
          {#each Object.entries(dialects) as [dialect, value]}
            <div class={"mb-0 flex items-baseline"}>
              <span class="mr-2 px-2 py-1 font-bold">{dialect}</span>
              <span class={"prose"}>
                {value.join("、")}
              </span>
            </div>
          {/each}
        </div>
      </Property>
    {/if}
  </div>
{/each}
