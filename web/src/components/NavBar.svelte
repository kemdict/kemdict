<script lang="ts">
  import NavBarButton from "$src/components/NavBarButton.svelte";
  import IconHomeOutline from "$src/components/IconHomeOutline.svelte";
  import IconHome from "$src/components/IconHome.svelte";
  import IconStarOutline from "$src/components/IconStarOutline.svelte";
  import IconSearchOutline from "$src/components/IconSearchOutline.svelte";
  import IconHistory from "$src/components/IconHistory.svelte";

  const { class: klass } = $props();
</script>

<div class={klass}>
  <NavBarButton href="/" text="首頁">
    <IconHome slot="active" />
    <IconHomeOutline slot="normal" />
  </NavBarButton>
  <NavBarButton href="/search" text="搜尋">
    <IconSearchOutline slot="active" />
    <IconSearchOutline slot="normal" />
  </NavBarButton>
  <NavBarButton href="/favorites" text="收藏">
    <IconStarOutline slot="active" />
    <IconStarOutline slot="normal" />
  </NavBarButton>
  <NavBarButton href="/history" text="紀錄">
    <IconHistory slot="active" />
    <IconHistory slot="normal" />
  </NavBarButton>
</div>
