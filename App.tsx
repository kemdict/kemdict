import { StatusBar } from "expo-status-bar";
import { StyleSheet, SafeAreaView, FlatList, ScrollView } from "react-native";

import Page from "./Page.tsx";

export default function App() {
  return (
    <SafeAreaView style={styles.container}>
      <Page />
      <StatusBar style="auto" />
    </SafeAreaView>
  );
}

const styles = StyleSheet.create({
  container: {
    // flex: 1,
    // backgroundColor: "#0B0F14",
    // alignItems: "center",
    // justifyContent: "center",
  },
  text: {
    // color: "#fff",
  },
});
