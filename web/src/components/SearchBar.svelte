<script>
  import clsx from "clsx";
  import { ListBox, ListBoxItem } from "@skeletonlabs/skeleton";
  export let initialInput = "";
  export let initialMatchSelection = "prefix";
  export let showMatchTypes = false;
  export let highlightBtn = false;
  // export let currentLang = "zh_TW";

  let currentMtch = initialMatchSelection;
  const matchTypes = [
    { mtch: "prefix", name: "開頭" },
    { mtch: "suffix", name: "結尾" },
    { mtch: "contains", name: "包含" },
    { mtch: "exact", name: "完全符合" },
  ];
</script>

<div class="relative mb-2 mt-2">
  <form action="/search">
    <div class="flex space-x-2">
      <div class="relative flex-grow">
        <input
          type="search"
          autocomplete="off"
          placeholder="輸入要搜尋的詞彙…"
          name="q"
          class="text-input w-full"
          value={initialInput}
        />
        <div class="absolute right-2 top-2">
          <kbd>Ctrl+K</kbd>
        </div>
      </div>
      <input
        class={clsx(
          "rounded-md p-1 px-2 text-sm font-bold transition",
          highlightBtn
            ? [
                "bg-indigo-500 text-indigo-50 hover:bg-indigo-600",
                "dark:bg-indigo-500 dark:hover:bg-indigo-400",
              ]
            : "btnColor btnBorder"
        )}
        type="submit"
        value="搜尋"
      />
    </div>
    {#if showMatchTypes}
      <div class="max-w-fit">
        <ListBox>
          {#each matchTypes as { mtch, name }}
            <ListBoxItem bind:group={currentMtch} name="m" value={mtch}>
              {name}
            </ListBoxItem>
          {/each}
        </ListBox>
      </div>
    {/if}
  </form>
</div>
