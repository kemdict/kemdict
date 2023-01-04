<script>
  // sure...
  export let limit = undefined;

  import kisaragi_dict from "$lib/kisaragi_dict.json";
  import WordList from "./WordList.svelte";

  const words = kisaragi_dict
    .sort((a, b) => a.added < b.added)
    .slice(0, limit)
    .map((obj) => obj.title);
</script>

<WordList {words} />
