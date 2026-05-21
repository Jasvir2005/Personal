import axios from "axios";
import { useState } from "react";
import { useNavigate } from "react-router-dom";
import "./login.css";

export default function Login() {
  const [data, setData] = useState({
    email: "",
    password: "",
  });

  const nav = useNavigate();

  const login = async () => {
    try {
      const res = await axios.post(
        "http://localhost:5000/auth/login",
        data
      );

      localStorage.setItem(
        "user",
        JSON.stringify(res.data.user)
      );
      localStorage.setItem(
        "token",
        res.data.token
      );

      nav("/chat");
    } catch (err) {
      console.log(err.response?.data);
      alert(
        err.response?.data?.message ||
          "Login failed"
      );
    }
  };

  return (
    <div className="login-wrapper">
      <div className="main-card">
        
        {/* LEFT SIDE */}
        <div className="left-side">
          <h1>Welcome!</h1>
          <div className="line"></div>

          <p>
            Connect with friends, chat instantly,
            share voice notes and enjoy your
            premium real-time messaging app.
          </p>

        </div>

        {/* RIGHT SIDE */}
        <div className="right-side">
          <div className="glass-card">
            <h2>Sign in</h2>

            <input
              type="email"
              placeholder="User Name / Email"
              value={data.email}
              onChange={(e) =>
                setData({
                  ...data,
                  email: e.target.value,
                })
              }
            />

            <input
              type="password"
              placeholder="Password"
              value={data.password}
              onChange={(e) =>
                setData({
                  ...data,
                  password: e.target.value,
                })
              }
            />

            <span
              className="forgot"
              onClick={() =>
                nav("/forgot-password")
              }
            >
              Forgot Password?
            </span>

            <button
              className="submit-btn"
              onClick={login}
            >
              Log in
            </button>

            <p
              className="signup"
              onClick={() => nav("/register")}
            >
              New here? Sign Up
            </p>
          </div>
        </div>
      </div>
    </div>
  );
}