<script lang="ts">
  export let presentLangs: string[] = [];
  export let requestedLang: string;
  export let title: string;
  export let currentParamString: string;
  const currentParams = new URLSearchParams(currentParamString);

  function paramsToString(langId: string) {
    if (currentParams) {
      currentParams.set("lang", langId);
    }
    return "?" + currentParams.toString();
  }

  import {
    popup,
    Tab,
    TabGroup,
    ListBox,
    ListBoxItem,
  } from "@skeletonlabs/skeleton";
  import { langs } from "common";
</script>

<TabGroup padding="">
  {#each presentLangs.slice(0, 7) as langId}
    <Tab group={requestedLang} name={langId} value={langId}>
      <a class="block px-4 py-2" href="/word/{title}{paramsToString(langId)}"
        >{langs[langId]}</a
      >
    </Tab>
  {/each}
  {#if presentLangs.length > 7}
    <div class="ml-2 flex items-center">
      <button
        class="btn btn-sm variant-filled"
        type="button"
        title="其他語言"
        use:popup={{
          event: "click",
          target: "popupLangs",
          placement: "bottom",
          closeQuery: ".listbox-item",
        }}
        >{presentLangs.indexOf(requestedLang) >= 7
          ? langs[requestedLang]
          : ""}…</button
      >
      <div class="card z-10 p-4 shadow-xl" data-popup="popupLangs">
        <ListBox padding="">
          {#each presentLangs.slice(7) as langId}
            <ListBoxItem group={requestedLang} name="lang" value={langId}>
              <a
                class="block px-4 py-2"
                href="/word/{title}{paramsToString(langId)}">{langs[langId]}</a
              >
            </ListBoxItem>
          {/each}
        </ListBox>
        <div class="arrow bg-surface-100-800-token" />
      </div>
    </div>
  {/if}
</TabGroup>
