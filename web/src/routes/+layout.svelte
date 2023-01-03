<script>
  import "../src.css";

  import { showLoadingAfterMS } from "$lib/common";
  import { navigating } from "$app/stores";
  import { fade } from "svelte/transition";
  import Loading from "./Loading.svelte";
</script>

{#if $navigating}
  <div transition:fade={{ delay: showLoadingAfterMS }}>
    <Loading />
  </div>
{/if}
<slot />
