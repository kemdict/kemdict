<script>
  import truncate from "lodash-es/truncate";
  import { spc } from "$lib/processing";
  import { dictsInWord } from "$lib/common";
  export let word;
  // FIXME: the results should be split on the server, not in WordPreview.
  // This would require flattening word objects into heteronyms during
  // processing.
  export let lang;
  // Needs to be reactive so that it can update dynamically
  $: presentDicts = dictsInWord(word, false, lang);
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
    return html?.replace(/<[^>]*>?/gm, "");
  }
  function processPreview(def) {
    return truncate(strip(def), { length: 45, omission: "……" });
  }
  function processPn(het) {
    let pn = het[pron_keys.find((pron) => het[pron])];
    if (pn) {
      return `（${spc(pn)}）`;
    } else {
      return "";
    }
  }
</script>

{#each presentDicts as dict}
  {#each word[dict].heteronyms as het}
    <li>
      <a href="/word/{word.title}">
        <div
          class="-mx-1 my-2 p-1 text-sm transition hover:bg-gray-100 dark:hover:bg-stone-800"
        >
          <h2 class="link font-bold hover:no-underline">
            {word.title}{processPn(het)}
          </h2>
          <p class="text-gray-500 dark:text-stone-300">
            {processPreview(
              het.definition ||
                het.definitions?.map((x) => x.def).join("") ||
                het.example
            )}
          </p>
        </div>
      </a>
    </li>
  {/each}
{/each}
