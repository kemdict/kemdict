<script>
  export let initialInput = "";
  export let initialMatchSelection = "prefix";
  export let showMatchTypes = false;

  const matchTypes = [
    { mtch: "prefix", name: "開頭" },
    { mtch: "suffix", name: "結尾" },
    { mtch: "contains", name: "包含" },
    { mtch: "exact", name: "完全符合" },
  ];

  /* import { lGet, lUpdate, lSet } from "$src/localStorage";
   * import { uniqWith, isEqual } from "lodash-es";
   * const sbi = document.getElementById("sbi") as HTMLInputElement;
   * const sf = document.getElementById("sf") as HTMLFormElement;
   * const sfd = document.getElementById("sfd") as HTMLSelectElement;
   * const sg = document.getElementById("suggest") as HTMLDivElement;
   * sf.addEventListener("submit", (_e) => {
   *   if (!lGet("search-history")) {
   *     lSet("search-history", []);
   *   }
   *   lUpdate("search-history", (old) => {
   *     const n = uniqWith(
   *       [{ q: sbi.value, m: sfd.selectedOptions[0].textContent }, ...old],
   *       isEqual
   *     );
   *     return n.slice(0, 5);
   *   });
   * });
   * sf.addEventListener("click", (e) => {
   *   e.stopPropagation();
   * });
   * window.addEventListener("keydown", (e) => {
   *   if (e.key === "Escape") {
   *     sg.classList.add("hidden");
   *   }
   * });
   * window.addEventListener("click", (_e) => {
   *   sg.classList.add("hidden");
   * });
   * sbi.addEventListener("focus", (_e) => {
   *   const ul = sg.getElementsByTagName("ul")[0] as HTMLUListElement;
   *   const h3 = document.createElement("h3") as HTMLHeadingElement;
   *   h3.textContent = "搜尋紀錄";
   *   h3.classList.add("text-gray-500", "dark:text-gray-300", "font-bold");
   *   const entries = lGet("search-history", []) as Array<{
   *     q: string;
   *     m: string;
   *   }>;
   *   // Invalid content -> clear it
   *   if (!entries[0]?.q) {
   *     lSet("search-history", []);
   *     return;
   *   }
   *   if (entries.length === 0) return;
   *   ul.replaceChildren(
   *     ...entries.map(({ q, m }) => {
   *       const li = document.createElement("li");
   *       const a = document.createElement("a");
   *       const mtch = document.createElement("span") as HTMLSpanElement;
   *       mtch.classList.add("text-gray-500", "dark:text-gray-400");
   *       mtch.textContent = `${m}：`;
   *       a.append(mtch);
   *       a.append(q);
   *       a.href = `/search?q=${q}`;
   *       a.classList.add("wordlink");
   *       li.classList.add("my-2", "mr-2");
   *       li.appendChild(a);
   *       return li;
   *     })
   *   );
   *   sg.replaceChildren(h3, ul);
   *   sg.classList.remove("hidden");
   * });
   * window.addEventListener("keydown", (e) => {
   *   // Following the same approach as
   *   // kit.svelte.dev/src/lib/search/SearchBox.svelte, with some hints
   *   // from MDN's page about navigator.platform
   *   let modifier = "ctrlKey";
   *   if (
   *     navigator.platform.indexOf("Mac") === 0 ||
   *     navigator.platform === "iPhone"
   *   ) {
   *     modifier = "metaKey";
   *   }
   *   if (e.key === "k" && e[modifier]) {
   *     sbi.focus();
   *     e.preventDefault();
   *   }
   * }); */
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
        class="btn p-1 px-2 text-sm font-bold"
        type="submit"
        value="搜尋"
      />
    </div>
    {#if showMatchTypes}
      <div class="max-w-fit">
        {#each matchTypes as { mtch, name }}
          <div>
            <label class="daisy-label cursor-pointer">
              <span class="daisy-label-text">{name}</span>
              <input
                checked={initialMatchSelection === mtch}
                type="radio"
                name="m"
                value={mtch}
                class="daisy-radio checked:bg-indigo-500"
              />
            </label>
          </div>
        {/each}
      </div>
    {/if}
  </form>
</div>
