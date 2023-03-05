<script>
  import truncate from "lodash-es/truncate";
  import { spc } from "$src/processing";
  export let heteronyms;
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
    let pn = het.props[pron_keys.find((pron) => het.props[pron])];
    if (pn) {
      return `（${spc(pn)}）`;
    } else {
      return "";
    }
  }
</script>

{#each heteronyms as het}
  <li>
    <a href="/word/{het.title}">
      <div
        class="-mx-1 my-2 p-1 text-sm transition hover:bg-gray-100 dark:hover:bg-stone-800"
      >
        <h2 class="link font-bold hover:no-underline">
          {het.title}{processPn(het)}
        </h2>
        <p class="text-gray-500 dark:text-stone-300">
          {processPreview(
            het.props.definition ||
              het.props.definitions?.map((x) => x.def).join("") ||
              het.props.example ||
              het.props.zh
          )}
        </p>
      </div>
    </a>
  </li>
{/each}
