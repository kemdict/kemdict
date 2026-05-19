<script lang="ts">
  const loading = !globalThis.location;
  const isDark = false;
  import { authClient } from "$lib/auth-client";
  const session = authClient.useSession();
  const klass =
    "btn h-5 bg-yellow-300 p-1 hover:bg-yellow-400 dark:bg-orange-900 dark:hover:bg-orange-800";
</script>

{#if loading}
  <div class={["w-5", klass]}></div>
{:else if $session.data}
  <button
    class={klass}
    type="button"
    title="logout"
    aria-label="logout"
    onclick={() => {
      authClient.signOut();
    }}
  >
    logged in as {$session.data.user.name}
  </button>
{:else}
  <button
    class={klass}
    type="button"
    title="login"
    aria-label="login"
    onclick={() => {
      authClient.signIn.social({ provider: "github" });
    }}
  >
    sign in with github
  </button>
{/if}
