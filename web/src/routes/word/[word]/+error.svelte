<script>
  import { page } from "$app/stores";
  import SearchBar from "$lib/SearchBar.svelte";
  import Elsewhere from "$lib/Elsewhere.svelte";
  import Header from "$lib/Header.svelte";
  const term = $page.params.word;
</script>

<svelte:head>
  <title>找不到「{term}」 - Kemdict 國語整合典</title>
</svelte:head>

<Header>
  <SearchBar />
</Header>

<div class="prose">
  <h1>{term}</h1>
  <p>找不到這個詞。</p>
  <p />
  <hr />
  <Elsewhere {term} />
</div>
