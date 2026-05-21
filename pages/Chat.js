/* eslint-disable react-hooks/exhaustive-deps */

import { useEffect, useState, useRef } from "react";
import axios from "axios";
import io from "socket.io-client";
import EmojiPicker from "emoji-picker-react";
import "./chat.css";

const socket = io("http://localhost:5000");

export default function Chat() {

  const bottomRef = useRef();
  const mediaRecorderRef = useRef(null);

  const user =
    JSON.parse(localStorage.getItem("user"));

  const [users, setUsers] = useState([]);
  const [selected, setSelected] =
    useState(null);

  const [messages, setMessages] =
    useState([]);

  const [isRecording, setIsRecording] = useState(false);

  const [msg, setMsg] = useState("");

  const [typing, setTyping] =
    useState("");

  const [onlineUsers, setOnlineUsers] =
    useState([]);

  const [showEmoji, setShowEmoji] =
    useState(false);
  const [editingId, setEditingId] = useState(null);
  const [editText, setEditText] = useState("");  

  // ================= AUTO SCROLL =================

  useEffect(() => {

    bottomRef.current?.scrollIntoView({
      behavior: "smooth"
    });

  }, [messages]);

  // ================= LOAD USERS =================

  useEffect(() => {

    if (!user) return;

    socket.emit("join", user._id);

    axios
      .get("http://localhost:5000/users")
      .then((res) => {

        const filtered =
          res.data.filter(
            (u) => u._id !== user._id
          );

        setUsers(filtered);

        if (filtered.length > 0) {
          setSelected(filtered[0]);
        }

      });

  }, []);

  // ================= LOAD CHAT =================

  useEffect(() => {

  if (!selected) return;

  axios
    .get(
      `http://localhost:5000/messages/${user._id}/${selected._id}`
    )
    .then((res) => {

      setMessages(res.data || []);

      // ✅ SEEN EVENT
      socket.emit("seen", {
        sender: user._id,
        receiver: selected._id
      });

    });

}, [selected]);

  // ================= RECEIVE MESSAGE =================

  useEffect(() => {

    const handleReceive = (data) => {

      if (
        selected &&
        (
          data.sender === selected._id ||
          data.receiver === selected._id
        )
      ) {

        setMessages((prev) => [
          ...prev,
          data
        ]);

      }

    };

    socket.on(
      "receiveMessage",
      handleReceive
    );

    return () => {

      socket.off(
        "receiveMessage",
        handleReceive
      );

    };

  }, [selected]);

  // ================= MESSAGE SEEN =================

useEffect(() => {

  socket.on("messagesSeen", () => {

    setMessages((prev) =>
      prev.map((m) => ({
        ...m,
        seen: true
      }))
    );

  });

  return () => {

    socket.off("messagesSeen");

  };

}, []);

  // ================= ONLINE USERS =================

  useEffect(() => {
  const handleOnline = (data) => {
    setOnlineUsers(data);
  };

  socket.on("onlineUsers", handleOnline);

  return () => {
    socket.off("onlineUsers", handleOnline);
  };
}, []);

  // ================= TYPING =================

  useEffect(() => {
  const handleTyping = (senderId) => {
    if (selected && senderId === selected._id) {
      setTyping("Typing...");

      setTimeout(() => {
        setTyping("");
      }, 1500);
    }
  };

  socket.on("typing", handleTyping);

  return () => {
    socket.off("typing", handleTyping);
  };
}, [selected]);
  // ================= SEND =================

  const send = () => {

    if (!msg.trim()) return;

    const newMessage = {

      sender: user._id,
      receiver: selected._id,
      text: msg,
      createdAt: new Date()

    };

    setMessages((prev) => [
      ...prev,
      newMessage
    ]);

    socket.emit(
      "sendMessage",
      newMessage
    );

    setMsg("");

  };

  // ================= IMAGE =================

  const sendImage = (e) => {

    const file = e.target.files[0];

    if (!file) return;

    const reader =
      new FileReader();

    reader.onloadend = () => {

      const newMessage = {

        sender: user._id,
        receiver: selected._id,
        image: reader.result,
        createdAt: new Date()

      };

      setMessages((prev) => [
        ...prev,
        newMessage
      ]);

      socket.emit(
        "sendMessage",
        newMessage
      );

    };

    reader.readAsDataURL(file);

  };

  // ================= VOICE =================

  const startRecording = async () => {
  try {
    if (!selected) return;

    const stream = await navigator.mediaDevices.getUserMedia({
      audio: true,
    });

    const mediaRecorder = new MediaRecorder(stream);
    mediaRecorderRef.current = mediaRecorder;

    const audioChunks = [];

    mediaRecorder.ondataavailable = (e) => {
      if (e.data.size > 0) {
        audioChunks.push(e.data);
      }
    };

    mediaRecorder.onstop = async () => {
  const audioBlob = new Blob(
    audioChunks,
    { type: "audio/webm" }
  );

  const formData = new FormData();
  formData.append(
    "audio",
    audioBlob,
    "voice.webm"
  );

  try {
    const uploadRes =
      await axios.post(
        "http://localhost:5000/upload/audio",
        formData
      );

    const newMessage = {
      sender: user._id,
      receiver: selected._id,
      audio: uploadRes.data.url,
      createdAt: new Date(),
    };

    setMessages((prev) => [
      ...prev,
      newMessage,
    ]);

    socket.emit(
      "sendMessage",
      newMessage
    );
  } catch (err) {
    console.log(err);
  }

  stream
    .getTracks()
    .forEach((t) => t.stop());
};

    mediaRecorder.start();
    setIsRecording(true);
  } catch (err) {
    console.log(err);
  }
};

const stopRecording = () => {
  if (
    mediaRecorderRef.current &&
    mediaRecorderRef.current.state !== "inactive"
  ) {
    mediaRecorderRef.current.stop();
  }
};

  // ================= INPUT =================

  const handleTypingInput = (e) => {

    setMsg(e.target.value);

    socket.emit("typing", user._id);

  };

  const deleteMessage = async (id) => {

  try {

    await axios.delete(
      `http://localhost:5000/message/${id}`
    );

    setMessages((prev) =>
      prev.filter((m) => m._id !== id)
    );

  } catch (err) {

    console.log(err);

  }

};

const deleteChat = async () => {

  try {

    await axios.put(

      "http://localhost:5000/delete-chat",

      {
        sender: user._id,
        receiver: selected._id
      }

    );

    // reload messages
    const res = await axios.get(

      `http://localhost:5000/messages/${user._id}/${selected._id}`

    );

    setMessages(res.data);

  } catch (err) {

    console.log(err);

  }

};

const editMessage = async (id) => {

  try {

    const res = await axios.put(
      `http://localhost:5000/message/${id}`,
      {
        text: editText
      }
    );

    setMessages((prev) =>
      prev.map((m) =>
        m._id === id
          ? res.data
          : m
      )
    );

    setEditingId(null);
    setEditText("");

  } catch (err) {

    console.log(err);

  }

};

  // ================= UI =================

  return (

    <div className="main-chat-container">

      {/* SIDEBAR */}

      <div className="left-sidebar">

        <div className="logo-box">
          CHAT
        </div>

        <div className="users-list">

          {users.map((u) => (

            <div
              key={u._id}
              className={`single-user ${
                selected?._id === u._id
                  ? "active-user"
                  : ""
              }`}
              onClick={() =>
                setSelected(u)
              }
            >

              <div className="user-left">

                <div className="avatar">

                  {u.profilePic ? (

                    <img
                      src={u.profilePic || u.image}
                      alt=""
                      className="profile-img"
                    />

                  ) : (

                    u.name[0]

                  )}

                </div>

                <div>

                  <h4>{u.name}</h4>

                  <p className={
                    onlineUsers.includes(u._id)
                      ? "online"
                      : "offline"
                    }>
                    {onlineUsers.includes(u._id)
                      ? "🟢 Online"
                      : "⚫ Offline"}
                    </p>

                </div>

              </div>

            </div>

          ))}

        </div>

      </div>

      {/* CHAT */}

      <div className="chat-section">

        {/* HEADER */}

        {selected && (

          <div className="chat-top">

            <div className="chat-user">

              <div className="avatar">

                {selected.profilePic ? (

                  <img
                    src={
                      selected.profilePic || selected.image
                    }
                    alt=""
                    className="profile-img"
                  />

                ) : (

                  selected.name[0]

                )}

              </div>

              <div>

                <h3>
                  {selected.name}
                </h3>

                <span>

                  {onlineUsers.includes(
                    selected._id
                  )
                    ? "🟢 Online"
                    : "⚫ Offline"}

                </span>

              </div>

            </div>
            <button className="delete-chat-btn" onClick={deleteChat}>
              Delete Chat
            </button>

          </div>

        )}

        {/* MESSAGES */}

        <div className="chat-messages">

          {messages.map((m, i) => (

  <div
    key={i}
    className={`msg ${
      m.sender === user._id
        ? "me"
        : "other"
    }`}
  >

    {/* EDIT MODE */}
    {editingId === m._id ? (

      <div className="edit-box">

        <input
          value={editText}
          onChange={(e) =>
            setEditText(e.target.value)
          }
        />

        <button
          onClick={() =>
            editMessage(m._id)
          }
        >
          Save
        </button>

      </div>

    ) : (

      <>

        {/* TEXT */}
        {m.text && (
          <p>{m.text}</p>
        )}

        {/* IMAGE */}
        {m.image && (

          <img
            src={m.image}
            alt=""
            className="chat-image"
          />

        )}

        {/* AUDIO */}
        {m.audio && (

          <audio controls>
            <source
              src={m.audio}
              type="audio/webm"
            />
          </audio>

        )}

        {/* TIME */}
        <span className="time">

          {m.createdAt
            ? new Date(
                m.createdAt
              ).toLocaleTimeString()
            : ""}

        </span>

        {/* SEEN */}
        {m.sender === user._id && (

          <span className="seen">
            {m.seen ? "✔✔" : "✔"}
          </span>

        )}

        {/* ACTION BUTTONS */}
        {m.sender === user._id && (

          <div className="msg-actions">

            {/* EDIT */}
            <button
              onClick={() => {

                setEditingId(m._id);
                setEditText(m.text);

              }}
            >
              ✏
            </button>

            {/* DELETE */}
            <button
              onClick={() =>
                deleteMessage(m._id)
              }
            >
              🗑
            </button>

          </div>

        )}

      </>

    )}

  </div>

))}

          <div ref={bottomRef}></div>

        </div>

        {/* TYPING */}

        {typing && (

          <div className="typing-box">
            {typing}
          </div>

        )}

        {/* INPUT */}

        <div className="chat-input-area">

          {/* EMOJI */}

          <button
            className="icon-btn"
            onClick={() =>
              setShowEmoji(!showEmoji)
            }
          >
            😊
          </button>

          {showEmoji && (

            <div className="emoji-picker">

              <EmojiPicker
                onEmojiClick={(e) =>
                  setMsg(
                    (prev) =>
                      prev + e.emoji
                  )
                }
              />

            </div>

          )}

          {/* IMAGE */}

          <label className="icon-btn">
            📷

            <input
              type="file"
              hidden
              accept="image/*"
              onChange={sendImage}
            />

          </label>

          {/* INPUT */}

          <input
            type="text"
            value={msg}
            onChange={
              handleTypingInput
            }
            placeholder="Type message..."
          />

          {/* MIC */}

          <button
  className="icon-btn"
  onClick={
    isRecording
      ? stopRecording
      : startRecording
  }
>
  {isRecording ? "⏹" : "🎤"}
</button>

          {/* SEND */}

          <button
            className="send-btn"
            onClick={send}
          >
            ➤
          </button>

        </div>

      </div>

      {/* PROFILE */}

      {selected && (

        <div className="right-profile">

          <div className="profile-card">

            <div className="big-avatar">

              {selected.profilePic ? (

                <img
                  src={
                    selected.profilePic || selected.image
                  }
                  alt=""
                  className="big-profile-img"
                />

              ) : (

                selected.name[0]

              )}

            </div>

            <h2>
              {selected.name}
            </h2>

            <p>
              Web Developer
            </p>

            <p>
              📍 Punjab, India
            </p>

          </div>

        </div>

      )}

    </div>

  );

}