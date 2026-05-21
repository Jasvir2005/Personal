import axios from "axios";
import { useState } from "react";
import { useNavigate } from "react-router-dom";
import "./login.css"; // same css use hovegi

export default function Register() {
  const [data, setData] = useState({
    name: "",
    email: "",
    password: "",
  });

  const nav = useNavigate();

  const register = async () => {
    try {
      await axios.post(
        "http://localhost:5000/auth/register",
        data
      );

      alert("Registered Successfully");
      nav("/");
    } catch (err) {
      console.log(err.response?.data);
      alert(
        err.response?.data?.message ||
          "Registration Failed"
      );
    }
  };

  return (
    <div className="login-wrapper">
      <div className="main-card">
        
        {/* LEFT SIDE */}
        <div className="left-side">
          <h1>Create Account!</h1>
          <div className="line"></div>

          <p>
            Join our real-time chat app and
            connect with friends instantly,
            send messages, audio and enjoy
            premium experience.
          </p>

          <button
            className="learn-btn"
            onClick={() => nav("/")}
          >
            Login Now
          </button>
        </div>

        {/* RIGHT SIDE */}
        <div className="right-side">
          <div className="glass-card">
            <h2>Sign Up</h2>

            <input
              type="text"
              placeholder="Full Name"
              value={data.name}
              onChange={(e) =>
                setData({
                  ...data,
                  name: e.target.value,
                })
              }
            />

            <input
              type="email"
              placeholder="Email"
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

            <button
              className="submit-btn"
              onClick={register}
            >
              Register
            </button>

            <p
              className="signup"
              onClick={() => nav("/")}
            >
              Already have account? Login
            </p>
          </div>
        </div>
      </div>
    </div>
  );
}