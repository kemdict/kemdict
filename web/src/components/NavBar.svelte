<script lang="ts">
  import NavBarButton from "$src/components/NavBarButton.svelte";
  import IconHomeOutline from "$src/components/IconHomeOutline.svelte";
  import IconHome from "$src/components/IconHome.svelte";
  import IconStarOutline from "$src/components/IconStarOutline.svelte";
  import IconSearchOutline from "$src/components/IconSearchOutline.svelte";
  import IconHistory from "$src/components/IconHistory.svelte";

  interface Props {
    class: string;
    pathname: string;
  }
  const { class: klass, pathname }: Props = $props();
</script>

<div class={klass}>
  <NavBarButton {pathname} href="/" text="首頁">
    <IconHome slot="active" />
    <IconHomeOutline slot="normal" />
  </NavBarButton>
  <NavBarButton {pathname} href="/search" text="搜尋">
    <IconSearchOutline active={true} slot="active" />
    <IconSearchOutline slot="normal" />
  </NavBarButton>
  <NavBarButton {pathname} href="/favorites" text="收藏">
    <IconStarOutline active={true} slot="active" />
    <IconStarOutline slot="normal" />
  </NavBarButton>
  <NavBarButton {pathname} href="/history" text="紀錄">
    <IconHistory active={true} slot="active" />
    <IconHistory slot="normal" />
  </NavBarButton>
</div>
