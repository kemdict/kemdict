<script>
  import { popup, ListBox, ListBoxItem } from "@skeletonlabs/skeleton";
  import { onMount } from "svelte";
  export let submitSuffix = "";
  export let initialInput = "";
  export let initialMatchSelection = "prefix";
  export let highlightBtn = false;

  import PopupMenu from "./PopupMenu.svelte";

  let currentMtch = initialMatchSelection || "prefix";
  const matchTypes = new Map(
    Object.entries({
      prefix: "開頭為",
      suffix: "結尾為",
      contains: "包含",
      exact: "完全符合",
    })
  );
  onMount(() => {
    window.addEventListener("keydown", (e) => {
      // Following the same approach as
      // kit.svelte.dev/src/lib/search/SearchBox.svelte, with some hints
      // from MDN's page about navigator.platform
      let modifier = "ctrlKey";
      if (
        navigator.platform.indexOf("Mac") === 0 ||
        navigator.platform === "iPhone"
      ) {
        modifier = "metaKey";
      }
      if (e.key === "k" && e[modifier]) {
        document.getElementById("sbi").focus();
        e.preventDefault();
      }
    });
  });
</script>

<div class="relative mb-2 mt-2">
  <form action="/search{submitSuffix || ''}">
    <div class="flex space-x-2">
      <div>
        <PopupMenu
          target="combo"
          btnClass="btn variant-filled h-full w-[10ch] text-sm"
          closeQuery=".listbox-item"
          label={matchTypes.get(currentMtch)}
        >
          <ListBox>
            {#each [...matchTypes] as [mtch, name]}
              <ListBoxItem bind:group={currentMtch} name="m" value={mtch}>
                {name}
              </ListBoxItem>
            {/each}
          </ListBox>
        </PopupMenu>
      </div>
      <div class="relative flex-grow">
        <input
          id="sbi"
          type="search"
          autocomplete="off"
          placeholder="輸入要搜尋的詞彙…"
          name="q"
          class="text-input w-full"
          value={initialInput}
        />
        <div class="absolute right-2 top-[25%]">
          <kbd>Ctrl+K</kbd>
        </div>
      </div>
      <input
        class={highlightBtn ? "k-hlbtn" : "k-btn"}
        type="submit"
        value="搜尋"
      />
    </div>
  </form>
</div>
