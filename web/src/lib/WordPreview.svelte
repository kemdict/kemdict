<script>
  import truncate from "lodash-es/truncate";
  import { spc } from "$lib/processing";
  import { dictsInWord } from "$lib/common";
  export let word;
  // Needs to be reactive so that it can update dynamically
  $: presentDicts = dictsInWord(word);
  // FIXME: for Hakkadict, it's questionable for me to pick one
  // dialect out of the six provided.
  const pron_keys = [
    "bopomofo",
    "trs",
    "pronunciation",
    "p_四縣",
    "poj",
    "kip",
  ];
  function strip(html) {
    // https://stackoverflow.com/a/822464/6927814
    // This doesn't have to be perfect. We're not handling untrusted
    // input either.
    return html.replace(/<[^>]*>?/gm, "");
  }
  function processPreview(def) {
    return truncate(strip(def), { length: 45, omission: "……" });
  }
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
            {processPreview(
              het.definition || het.definitions.map((x) => x.def).join("")
            )}
          </p>
        </div>
      </a>
    </li>
  {/each}
{/each}
