import { useEffect, useState } from "react";
import axios from "axios";
import { useNavigate } from "react-router-dom";
import "./chat.css";

export default function Contacts() {
  const [users, setUsers] = useState([]);
  const [search, setSearch] = useState("");
  const nav = useNavigate();

  const currentUser = JSON.parse(localStorage.getItem("user"));

  useEffect(() => {
    const fetchUsers = async () => {
      try {
        const res = await axios.get(
          "http://localhost:5000/auth/users"
        );

        const filteredUsers = res.data.filter(
          (user) => user._id !== currentUser?._id
        );

        setUsers(filteredUsers);
      } catch (err) {
        console.log(err);
      }
    };

    fetchUsers();
  }, [currentUser?._id]);

  const filteredUsers = users.filter((user) =>
    user.name
      .toLowerCase()
      .includes(search.toLowerCase())
  );

  return (
    <div className="contacts-container">
      <h2>Contacts</h2>

      <input
        className="search-input"
        placeholder="Search contacts..."
        value={search}
        onChange={(e) => setSearch(e.target.value)}
      />

      {filteredUsers.length === 0 ? (
        <p className="no-users">No contacts found</p>
      ) : (
        filteredUsers.map((user) => (
          <div
            className="contact-card"
            key={user._id}
            onClick={() => nav(`/chat/${user._id}`)}
          >
            <div className="nav-avatar">
              {user?.name?.[0]?.toUpperCase()}
            </div>

            <div className="contact-info">
              <h4>{user.name}</h4>
              <p>{user.email}</p>
            </div>

            {/* online status later */}
            <div className="status-box">
              <span className="offline-dot"></span>
            </div>
          </div>
        ))
      )}
    </div>
  );
}