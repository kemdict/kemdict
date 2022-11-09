<script>
  import { goto } from "$app/navigation";
  import { onMount } from "svelte";
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
 onMount(() => {
   const esc = (event) => {
     if (event.key === "Escape") {
       setHidden(resultsList, true);
     }
   }
   const outsideClick = () => {
     setHidden(resultsList, true);
   }
   document.addEventListener("click", outsideClick);
   document.addEventListener("keyup", esc);
   return () => {
     document.removeEventListener("click", outsideClick)
     document.removeEventListener("keyup", esc)
   }
 })
</script>

<div id="sbc" on:click|stopPropagation>
  <form
    on:submit|preventDefault={() => {
      goto(`/word/${needle}`);
    }}
    id="sf"
  >
    <input
      id="sb"
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
