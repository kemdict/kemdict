<script>
  import { page } from "$app/stores";
  import Elsewhere from "$lib/Elsewhere.svelte";
  import SingleLayout from "$lib/SingleLayout.svelte";
  const term = $page.params.word;
</script>

<svelte:head>
  <title>找不到「{term}」 - Kemdict 國語整合典</title>
</svelte:head>

<SingleLayout class="prose">
  <h1>{term}</h1>
  <p>找不到這個詞。</p>
  <p />
  <hr />
  <Elsewhere {term} />
</SingleLayout>
