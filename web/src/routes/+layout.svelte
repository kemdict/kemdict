<script>
  import "../src.css";
  import { navigating } from "$app/stores";
  import { fade } from "svelte/transition";
  import Loading from "./Loading.svelte";
</script>

{#if $navigating}
    <!-- <div out:fade={{ delay: 250 }}>
         <Loading />
         </div> -->
{/if}

<slot />
