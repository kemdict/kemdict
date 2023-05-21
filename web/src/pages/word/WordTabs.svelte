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

  import { Tab, TabGroup } from "@skeletonlabs/skeleton";
  import { langs } from "common";
</script>

<TabGroup>
  {#each presentLangs as langId}
    <Tab group={requestedLang} name={langId} value={langId}>
      <a class="block px-4 py-2" href="/word/{title}{paramsToString(langId)}"
        >{langs[langId]}</a
      >
    </Tab>
  {/each}
</TabGroup>
