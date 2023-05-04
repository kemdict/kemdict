import { StatusBar } from "expo-status-bar";
import { StyleSheet, Text, View, SafeAreaView, FlatList } from "react-native";
import * as FS from "expo-file-system";
import * as SQLite from "expo-sqlite";
import { Asset } from "expo-asset";
import { useEffect, useState } from "react";
import useSWR from "swr";

/**
 * Read the database.
 */
async function readDB(): Promise<SQLite.WebSQLDatabase> {
  let info: FS.FileInfo;
  info = await FS.getInfoAsync(FS.documentDirectory + "SQLite/entries.db");
  if (info.exists) {
    await FS.deleteAsync(FS.documentDirectory + "SQLite/entries.db");
  }
  info = await FS.getInfoAsync(FS.documentDirectory + "SQLite");
  if (!info.exists) {
    await FS.makeDirectoryAsync(FS.documentDirectory + "SQLite");
  }
  info = await FS.getInfoAsync(FS.documentDirectory + "SQLite/entries.db");
  if (!info.exists) {
    console.log("downloading");
    await FS.downloadAsync(
      // require only works with static values. (On Hermes only, I guess??)
      Asset.fromModule(require("./assets/entries.db")).uri,
      FS.documentDirectory + "SQLite/entries.db"
    );
    console.log("done");
  }
  return SQLite.openDatabase("entries.db");
}

export default function App() {
  const { data, isLoading } = useSWR("dummy", async () => {
    const db = await readDB();
    return await new Promise((resolve) => {
      db.transaction((tx) => {
        tx.executeSql(
          `select * from "heteronyms" limit 10;`,
          [],
          (_, resultSet) => {
            resolve(resultSet.rows._array);
          },
          (_, _err) => {
            resolve([{ title: "error" }]);
          }
        );
      });
    });
  });
  return (
    <SafeAreaView style={styles.container}>
      <Text style={{ color: "#fff" }}>Hello!</Text>
      {isLoading ? (
        <View>
          <Text style={{ color: "#fff" }}>Loading</Text>
        </View>
      ) : (
        <FlatList
          data={data}
          renderItem={({ item }) => (
            <View>
              <Text style={{ color: "#fff" }}>{JSON.stringify(item)}</Text>
            </View>
          )}
          keyExtractor={(item) => item.title}
        ></FlatList>
      )}
      <StatusBar style="auto" />
    </SafeAreaView>
  );
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    backgroundColor: "#0B0F14",
    alignItems: "center",
    justifyContent: "center",
  },
});
