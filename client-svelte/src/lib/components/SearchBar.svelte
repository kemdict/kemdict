<script>
  import { goto } from "$app/navigation";
  import titles from "$lib/titles.json";
  const Match = {
    includes: (str, needle) => str.indexOf(needle) === -1,
    startsWith: (str, needle) => str.startsWith(needle),
    endsWith: (str, needle) => str.endsWith(needle),
  };
  let matchFunc = Match.startsWith;
  let needle = "";
  $: matching = (() => {
    if (needle.trim().length > 0) {
      return titles.filter((title) => matchFunc(title, needle.trim()));
    } else {
      return false;
    }
  })();
  let resultsList;
  function setHidden(elem, hide) {
    if (hide) {
      elem.classList.remove("visible");
      elem.classList.add("invisible");
    } else {
      elem.classList.remove("invisible");
      elem.classList.add("visible");
    }
  }
</script>

<div id="sbc">
  <form
    on:submit|preventDefault={() => {
      goto(`/word/${needle}`);
    }}
    id="sf"
  >
    <input
      id="sb"
      on:focus={setHidden(resultsList, false)}
      on:blur={setHidden(resultsList, true)}
      type="search"
      autocomplete="off"
      placeholder="輸入詞彙"
      bind:value={needle}
    />
    <input type="submit" value="前往" />
  </form>
  <ul id="sr" bind:this={resultsList}>
    {#if matching}
      <p>共 {matching.length} 條相符條目</p>
      {#each matching as w}
        <li><a href="/word/{w}">{w}</a></li>
      {/each}
    {/if}
  </ul>
</div>
