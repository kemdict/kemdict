<script>
  import truncate from "lodash-es/truncate";
  import { dicts } from "$lib/common";
  import { spc } from "$lib/processing";
  export let word;
  // Needs to be reactive so that it can update dynamically
  $: presentDicts = Object.keys(dicts).filter((x) => word[x]);
  // FIXME: for Hakkadict, it's questionable for me to pick one
  // dialect out of the six provided.
  const pron_keys = ["bopomofo", "trs", "pronunciation", "p_四縣"];
</script>

{#each presentDicts as dict}
  {#each word[dict].heteronyms as het}
    <li>
      <a href="/word/{word.title}">
        <div
          class="text-sm transition hover:bg-gray-100 dark:hover:bg-stone-800 p-1 -mx-1 my-2"
        >
          <h2 class="link hover:no-underline font-bold">
            {word.title}（{spc(het[pron_keys.find((pron) => het[pron])])}）
          </h2>
          <p class="text-gray-500 dark:text-stone-300">
            {truncate(
              het.definition || het.definitions.map((x) => x.def).join(""),
              { length: 40, omission: "……" }
            )}
          </p>
        </div>
      </a>
    </li>
  {/each}
{/each}
