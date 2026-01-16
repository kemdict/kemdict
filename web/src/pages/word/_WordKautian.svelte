<script lang="ts">
  import type { Heteronym } from "common";
  import type { OutputWord } from "$dicts/ministry-of-education/kautian.ts";
  let { heteronyms }: { heteronyms: Array<Heteronym<OutputWord>> } = $props();
  import Pronunciation from "$src/components/Pronunciation.svelte";
  import Property from "$src/components/Property.svelte";
  import { groupByProp } from "common";

  let prev單字不成詞 = $state(false);
</script>

{#each heteronyms as kautianWord, i}
  {@const this單字不成詞 = kautianWord.props.type === "單字不成詞者"}
  {@const this臺華共同詞 = kautianWord.props.type === "臺華共同詞"}

  <div id="kautian-word-{kautianWord.props.id}">
    <!-- 這個是單字不成詞者的話，如果前一個也是那就不要再顯示一次標題 -->
    {#if !this單字不成詞 || !prev單字不成詞}
      <h1>{kautianWord.title}</h1>
    {/if}
    <Pronunciation>{kautianWord.props.tl.main}</Pronunciation>
    <Property key="異用字" value={kautianWord.props.han.alt?.join("、")}
    ></Property>
    <!-- 在有一群單字不成詞的詞目時，只在最後一個顯示無義項的說明 -->
    {#if this單字不成詞 || this臺華共同詞}
      {@const nextType = heteronyms[i + 1]?.props.type}
      {@const next單字不成詞 = nextType === "單字不成詞者"}
      {@const next臺華共同詞 = nextType === "臺華共同詞"}
      {#if !next單字不成詞}
        <p class="def">（單字不成詞者 ，無義項）</p>
      {:else if !next臺華共同詞}
        <p class="def">（臺華共同詞，無義項）</p>
      {/if}
    {/if}
    {#if (prev單字不成詞 = this單字不成詞)}{/if}

    {#each groupByProp(kautianWord.props.heteronyms || [], "pos", "none") as [pos, hets]}
      {#if pos !== "none"}
        <p class="pos">{pos}</p>
      {/if}
      <ol>
        {#each hets as kautianHet}
          <li id="kautian-het-{kautianHet.id}">
            <p class="def">
              {kautianHet.def}
            </p>
            {#each kautianHet.examples as example}
              <blockquote>
                <p>{example.han}</p>
                <p>{example.tl}</p>
                <p class="pt-1 opacity-80">({example.zh})</p>
              </blockquote>
            {/each}
          </li>
        {/each}
      </ol>
    {/each}

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
