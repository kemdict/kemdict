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
  let info = await FS.getInfoAsync(FS.documentDirectory + "SQLite");
  if (!info.exists) {
    await FS.makeDirectoryAsync(FS.documentDirectory + "SQLite");
  }
  info = await FS.getInfoAsync(FS.documentDirectory + "SQLite/entries.db");
  if (!info.exists) {
    console.log("starting download");
    await FS.downloadAsync(
      // require only works with static values. (On Hermes only, I guess??)
      Asset.fromModule(require("./entries.db")).uri,
      FS.documentDirectory + "SQLite/entries.db"
    );
    console.log("download finished");
  }
  return SQLite.openDatabase("SQLite/entries.db");
}

export default function App() {
  const { data, isLoading } = useSWR("dummy", async () => {
    const db = await readDB();
    console.log("db acquired");
    return await new Promise((resolve) => {
      db.transaction((tx) => {
        console.log("executing SQL");
        tx.executeSql(
          `select * from "heteronyms" where "title" = ?;`,
          ["㐀"],
          (_, resultSet) => {
            console.log("success");
            resolve(resultSet.rows._array);
          },
          (_, _err) => {
            console.log("error");
            resolve([{ title: "error" }]);
          }
        );
      });
    });
  });
  console.log(`data: ${data}`);
  console.log(`isLoading: ${isLoading}`);
  return (
    <SafeAreaView style={styles.container}>
      <Text style={{ color: "#fff" }}>Hello!</Text>
      {!data ? (
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
